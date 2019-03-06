{-# LANGUAGE TupleSections #-}
module AssociativeSearchNetwork ( fig12
                                , Weight
                                , TimeStamp
                                , fig4
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

data Configs = C { mean :: !Double
                 , stDev :: !Double
                 , distribution :: ![(Double, Rational)]
                 , alfa :: !Double
                 , signals :: ![(SignalGen, Weight -> Weight)]
                 , environment :: TimeStamp -> [Double] -> Double
                 }
data AdaptiveElement = AE { weights :: ![Weight]
                          , inputTraces :: ![InTrace]
                          , lRate :: TimeStamp -> Double
                          , output :: !Double
                          , prevOutput :: !Double
                          , prevPrevOutput :: !Double
                          }

data Signal = ON | OFF deriving Show
type TimeStamp = Int
type SignalGen = TimeStamp -> Signal
type Stack g a = RandT g (Reader Configs) a

printD :: Double -> String 
printD = printf "%.4f"

debug :: [Double] -> String -> [Double]
debug xs l | trace (l ++ foldl (\s d -> s ++ printD d ++ ", ") "[" xs ++ "]") False = undefined
debug xs l = xs

debugD :: Double -> String -> Double
debugD d l | trace (l ++ printD d) False = undefined
debugD d l = d

debugS :: Show a => String ->  a -> a
debugS l d | trace (l ++ show d) False = undefined
debugS l d = d

positive :: Double -> Double
positive v
  | v > 0.0 = 1.0
  | otherwise = 0.0

normal :: Double -> Double -> [(Double, Rational)]
normal m sd = loop 1 $ negate r
  where
    r = sd
    step :: Int -> Int
    step n = div n 50
    loop :: Int -> Double -> [(Double, Rational)]
    loop n v
      | v <= m = (v, toRational n) : loop (n + step n) (v + 0.001)
      | v > m && v <= r = (v, toRational n) : loop (n - step n) (v + 0.001)
      | v > r = []

noise :: RandomGen g => Output -> Stack g Double
noise x = do
  d <- asks distribution
  n <- fromList d
  return $ x + n

sumProds :: [Double] -> [Double] -> Double
sumProds xs ys = loop xs ys 0.0
  where
    loop :: [Double] -> [Double] -> Double ->  Double
    loop [] [] s = s
    loop (w:ws) (x:xs) s = loop ws xs (s + w*x)

calcOutput :: RandomGen g => [Input] -> [Weight] -> Stack g Double
calcOutput xs ws = do
  y <- noise $ sumProds ws xs
  return $ positive y

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

repl :: a -> Stack g [a]
repl a = do 
  sgs <- asks signals
  return $ replicate (length sgs) a

inputs :: TimeStamp -> Stack g [Input]
inputs t = do
  sgs <- asks signals
  return $ fmap (toDouble . ($ t) . fst) sgs

tl :: String -> TimeStamp -> String
tl pre t = pre ++ "(" ++ show t ++ ") = "

adaptOutput :: RandomGen g => TimeStamp -> AdaptiveElement -> Stack g AdaptiveElement
adaptOutput t ae = do
  xs <- inputs (t-1)
  xts <- calcInputTraces xs $ inputTraces ae
  y <- calcOutput (debug xs $ tl "xs" (t-1)) $ weights ae
  return AE { weights = weights ae
            , inputTraces = xts
            , output = y
            , prevOutput = output ae
            , prevPrevOutput = prevOutput ae
            , lRate = lRate ae
            }

adaptWeight :: Double -> TimeStamp -> AdaptiveElement -> Stack g AdaptiveElement
adaptWeight zDiff t ae = do
  fs <- asks $ fmap snd . signals
  let
    yDiff = prevOutput ae - prevPrevOutput ae
    loop :: [Weight] -> [InTrace] -> [Weight -> Weight] -> [Weight]
    loop [] [] [] = []
    loop (w:ws) (xt:xts) (f:fs) = f (w + lRate ae t *(debugD zDiff "zDiff = ")*(debugD yDiff "yDiff = ")*xt) : loop ws xts fs
    ws = loop (weights ae) (inputTraces ae) fs
  return AE { weights = debug ws $ tl "ws" t
            , inputTraces = inputTraces ae
            , output = output ae
            , prevOutput = prevOutput ae
            , prevPrevOutput = prevPrevOutput ae
            , lRate = lRate ae
            }

asn :: Configs -> [AdaptiveElement] -> ((TimeStamp, [AdaptiveElement]) -> Bool) -> IO ([(TimeStamp, Double)], [AdaptiveElement])
asn cs aes until = do
  gen <- newStdGen
  return $ runReader (flip evalRandT gen $ search 0 [] aes) cs
    where
      search :: RandomGen g => TimeStamp -> [(TimeStamp, Double)] -> [AdaptiveElement] -> Stack g ([(TimeStamp, Double)], [AdaptiveElement])
      search t ts aes= do
        env <- asks environment
        aes' <- forM aes $ adaptOutput t
        let 
          z = env t (fmap prevOutput aes')
          zDiff = z - env (t-1) (fmap prevPrevOutput aes')
          ts' = (t, z):ts
        aes'' <- forM aes' $ adaptWeight zDiff t
        if until (t, aes'') then
          return (ts', aes'')
        else
          search (t+1) ts' aes''

initAE :: Int -> (TimeStamp -> Double) -> AdaptiveElement
initAE n lr = 
  let ds = replicate n 0.0
  in AE { weights = ds
        , inputTraces = ds
        , lRate = lr
        , output = 0.0
        , prevOutput = 0.0
        , prevPrevOutput = 0.0
        }
        
asnReset :: Int -> [SignalGen] -> (TimeStamp -> [Output] -> Double) -> IO [(TimeStamp, Double)]
asnReset n sgs env = fmap fst $ asn cs aes $ untilTS 1000
  where
    cs = C { mean = 0.0
           , stDev = 0.1
           , distribution = normal 0.0 0.1
           , alfa = 0.0
           , signals = fmap (,id) sgs
           , environment = env
           }
    resetLr t
      | t `mod` 10 == 0 = 0.0
      | otherwise = 0.03
    aes = replicate n $ initAE (length sgs) resetLr

contextSwitch :: a -> a -> a -> TimeStamp -> a
contextSwitch v1 v2 d t
  | t < 0 = d
  | div t 10 `mod` 2 == 0 = v1
  | otherwise = v2

fig4 :: IO [(TimeStamp, Double)]
fig4 = asnReset 9 signalSwitches env
  where
    env t ys =
      let z = sumProds (debug ys $ tl "ys" t)
      in contextSwitch (z (debug [-1,1,1,1,-1,1,1,1,-1] $ tl "ya" t)) (z (debug [1,1,-1,-1,1,-1,-1,1,1] $ tl "ya" t)) 0 t
    xs = replicate 4 ON ++ replicate 4 OFF
    xs' = reverse xs
    signalSwitch (x,x') = contextSwitch x x' OFF
    signalSwitches = signalSwitch <$> zip xs xs'

{-asnWithPred :: Int 
asnWithPred n = asn cs 
  where
    cs = C { mean = 0.0
           , stDev = 0.1
           , alfa = 0.0
          , signals = sgs
           , 
           }
    p = initAE 0.1
    aes = p : replicate n $ initAE $ const 0.03
-}

untilTS :: TimeStamp -> (TimeStamp, [AdaptiveElement]) -> Bool
untilTS t (t', _) = t == t'

untilAvgW :: Weight -> (TimeStamp, [AdaptiveElement]) -> Bool
untilAvgW w (_, aes) =  sum ws / fromIntegral (length ws) >= w
  where ws = join $ fmap weights aes

signalGen :: Int -> Int -> Int -> TimeStamp -> Signal
signalGen delay d td t = if t >= 0 && t' >= delay && t' <= delay+(d-1) then ON else OFF
  where t' = t `mod` td

fig12 :: IO (TimeStamp, [Weight])
fig12 = do
  (ts, ws) <- asn cs [initAE (length sgs) $ const 0.2] $ untilAvgW 0.6
  return (fst $ head ts, head $ fmap weights ws)
  where
    td = 3+20+27
    inSigGen = replicate 4 (signalGen 0 3 td, id)
    outSigGen = (signalGen 3 20 td, const 0.6)
    sgs = outSigGen:inSigGen
    cs = C { mean = 0.005
           , stDev = 0.03
           , distribution = normal 0.005 0.03
           , alfa = 0.9
           , signals = sgs
           , environment = const $ const 1.0
           }