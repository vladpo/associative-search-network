module AssociativeSearchNetwork ( fig12
                                , Weight
                                , TimeStamp
                                ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Control.Monad.Reader
import Control.Arrow
import System.Random
import Text.Printf
import Debug.Trace (trace)

type Weight = Double
type Input = Double
type InTrace = Double
type Output = Double

data Env = Env { mean :: Double
               , stDev :: Double
               , alfa :: Double
               , lr :: Double
               , signals :: [(SignalGen, Weight -> Weight)]
               }
data AdaptiveElement = AE { weights :: [Weight]
                          , inputTraces :: [InTrace]
                          , output :: Double
                          , prevOutput :: Double
                          }

data Signal = ON | OFF deriving Show
type TimeStamp = Int
type SignalGen = TimeStamp -> Signal
type Stack g a = RandT g (Reader Env) a

printD :: Double -> String 
printD = printf "%.4f"

debug :: [Double] -> [Double]
debug xs | trace (foldl (\s d -> s ++ printD d ++ ", ") "[" xs ++ "]") False = undefined
debug xs = xs

debugD :: Double -> Double
debugD d | trace (printD d) False = undefined
debugD d = d

positive :: Double -> Double
positive v
  | v > 0.0 = 1.0
  | otherwise = 0.0

normal :: Double -> Double -> [(Double, Rational)]
normal m sd = loop 1.0 $ negate r
  where
    r = 3.0*sd
    loop :: Rational -> Double -> [(Double, Rational)]
    loop n v
      | v <= m = (v, n) : loop (n+1) (v + 0.001)
      | v > m && v <= r = (v, n) : loop (n-1) (v + 0.001)
      | v > r = []

noise :: RandomGen g => Input -> Stack g Double
noise x = do
  (m, sd) <- asks $ mean &&& stDev
  n <- fromList $ normal m sd
  return $  min 1.0 (max 0.0 (x + n))

calcOutput :: [Input] -> [Weight] -> Stack g Double
calcOutput xs ws = return $ positive (sum ws xs 0.0)
  where
    sum :: [Weight] -> [Input] -> Double ->  Double
    sum [] [] s = s
    sum (w:ws) (x:xs) s = sum ws xs (s + w*x)

calcInputTraces :: [Input] -> [InTrace] -> Stack g [Double]
calcInputTraces xs xts = do
  a <- asks alfa
  let
    loop :: [InTrace] -> [Input] -> [Double]
    loop [] [] = []
    loop (xt:xts) (x:xs) = (a*xt + x) : loop xts xs
  return $ loop xts xs

toDouble :: Signal -> Double
toDouble ON = 1.0
toDouble OFF = 0.0

calcWeights :: AdaptiveElement -> Double -> Stack g [Weight]
calcWeights ae envResp = do
  (c, fs) <- asks $ lr &&& fmap snd . signals
  let
    yDiff = output ae - prevOutput ae
    loop :: [Weight] -> [InTrace] -> [Weight -> Weight] -> [Weight]
    loop [] [] [] = []
    loop (w:ws) (xt:xts) (f:fs) = f (w + c*envResp*yDiff*xt) : loop ws xts fs
  return $ loop (weights ae) (inputTraces ae) fs

repl :: a -> Stack g [a]
repl a = do 
  sgs <- asks signals
  return $ replicate (length sgs) a

inputs :: RandomGen g => TimeStamp -> Stack g [Input]
inputs t = do
  sgs <- asks signals
  let xs = fmap (toDouble . ($ t) . fst) sgs
  sequence $ fmap noise xs

adapt :: RandomGen g => Double -> TimeStamp -> AdaptiveElement -> Stack g AdaptiveElement
adapt envResp t ae = do
  xs <- inputs (t-1)
  xts <- calcInputTraces xs $ inputTraces ae
  xs' <- inputs t
  y <- calcOutput xs' $ weights ae
  ws <- calcWeights ae envResp
  return AE { weights = ws
            , inputTraces = xts
            , output = y
            , prevOutput = output ae
            }

predictor :: RandomGen g => [SignalGen] -> Double -> [Weight] -> Stack g Double
predictor sgs z ws = adapt z
  where
    env = Env { mean = 0.0
              , stDev = 0.01
              , alfa = 0.0
              , lr = 0.1
              , signals = sgs
              , output = const 1.0
              , until = undefined
              }

envFunc :: [Output] -> Double
envFunc ys = undefined

envResp :: [AdaptiveElement] -> Double
envResp = envFunc . fmap output

initAE :: AdaptiveElement
initAE = 
  let ds = repl 0.0
  in AE { weights = ds
        , inputTraces = ds
        , output = 0.0
        , prevOutput = 0.0
        }

asn :: Env -> Int -> ((TimeStamp, [AdaptiveElement]) -> Bool) -> IO (TimeStamp, [AdaptiveElement])
asn env n f = do
  gen <- newStdGen
  return $ runReader (flip evalRandT gen $ search 0 (pure $ replicate n initAE)) env
    where
      search ::  RandomGen g => (TimeStamp, Stack g [AdaptiveElement]) -> Stack g (TimeStamp, [AdaptiveElement])
      search (t, maes) = do
        aes <- maes
        z <- envResp aes
        let r = (t, aes)
        if f r then
          return r
        else
          search (t+1, forM aes $ adapt z t)
    

untilTS :: TimeStamp -> (TimeStamp, [Weight]) -> Bool
untilTS t (t', _) = t == t'

untilAvgW :: Weight -> (TimeStamp, [Weight]) -> Bool
untilAvgW w (_, ws) =  sum ws / fromIntegral (length ws) >= w

signalGen :: Int -> Int -> Int -> TimeStamp -> Signal
signalGen delay d td t = if t >= 0 && t' >= delay && t' <= delay+(d-1) then ON else OFF
  where t' = t `mod` td

fig12 :: IO (TimeStamp, [Weight])
fig12 = do
  (t, ws) <- asn env 1
  return (t, head $ fmap weights ws)
  where
    env = Env { mean = 0.005
              , stDev = 0.03
              , alfa = 0.9
              , lr = 0.2
              , signals = outSigGen:inSigGen
              }
    td = 3+20+27
    inSigGen = replicate 4 (signalGen 0 3 td, id)
    outSigGen = (signalGen 3 20 td, const 0.6)