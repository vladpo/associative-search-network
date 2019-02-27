module AdaptiveElement where

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import Control.Monad.Reader
import Control.Arrow
import qualified System.Random.MWC.Distributions as Dist
import qualified System.Random.MWC as MWC

import Debug.Trace (trace)

type Weight = Double
type Input = Double
type InTrace = Double
type AdaptiveElement = [Weight]

data Env = Env { mean :: Double
               , stDev :: Double
               , alfa :: Double
               , beta :: Double
               , lr :: Double
               , signals :: [(SignalGen, Weight -> Weight)]
               }
data Signal = ON | OFF
type TimeStamp = Int
type SignalGen = TimeStamp -> Signal
type Stack g a = RandT g (Reader Env) a

positive :: Double -> Double
positive v
  | v > 0.0 = 1.0
  | otherwise = 0.0

noise :: RandomGen g => Input -> Stack g Double
noise x = do
  (m, sd) <- asks $ mean &&& stDev
  n <- getRandomR (m - sd, m + sd)
  return (x - n)

output :: RandomGen g => [Weight] -> [Input] -> Stack g Double
output ws xs = fmap positive  (sum ws xs $ pure 0.0)
  where
    sum :: [Weight] -> [Input] -> Stack g Double -> Stack g Double
    sum [] [] ms = ms
    sum (w:ws) (x:xs) ms = do
      s <- ms
      nx <- noise x
      sum ws xs $ pure (s + w*nx)

inputTrace :: RandomGen g => (InTrace, Input) -> Stack g Double
inputTrace (xt, x) = do
  c <- asks lr
  return $ c*xt + x

toDouble :: Signal -> Double
toDouble ON = 1.0
toDouble OFF = 0.0

prev :: TimeStamp -> TimeStamp
prev t = t - 1

next :: TimeStamp -> TimeStamp
next t = t + 1

weights :: RandomGen g => [Weight] -> [InTrace] -> Double -> Double -> Stack g [Weight]
weights ws xts resDiff yDiff = do
  (c, fs) <- asks $ lr &&& fmap snd . signals
  let
    loop :: [Weight] -> [InTrace] -> [Weight -> Weight] -> [Weight]
    loop [] [] [] = []
    loop (w:ws) (xt:xts) (f:fs) = f (w + c*resDiff*yDiff*xt) : loop ws xts fs
  return $ loop ws xts fs

repl :: RandomGen g => a -> Stack g [a]
repl a = do 
  sgs <- asks signals
  return $ replicate (length sgs) a

initWeights :: RandomGen g => Stack g [Weight]
initWeights = repl 0.0

initInTraces :: RandomGen g => Stack g [InTrace]
initInTraces = repl 0.0

adapt :: RandomGen g => ((TimeStamp, [Weight]) -> Bool) -> Stack g [Weight]
adapt f = do
  ws <- initWeights
  xts <- initInTraces
  loop 0 ws xts 0.0 0.0 0.0
  where
    inputs :: TimeStamp -> Stack g [Input]
    inputs t = do
      sgs <- asks signals
      return $ fmap (toDouble . ($ t) . fst) sgs
    loop :: TimeStamp -> [Weight] -> [InTrace] -> Double -> Double -> Double -> Stack g [Weight]
    loop t ws xts y y' y'' = do
      xs <- inputs t
      ws' <- weights ws xts 1.0 (y''-y')
      ny <- output ws xs
      xs <- inputs (t-1)
      xts' <- sequence (inputTrace <$> zip xts xs)
      if f (t,ws') then
        return ws'
      else
        loop (t+1) ws' xts' ny y y'

untilTs :: TimeStamp -> (TimeStamp, [Weight]) -> Bool
untilTs t (t', _) = t == t'

signalGen :: Int -> Int -> Int -> TimeStamp -> Signal
signalGen delay d td t = if t' >= delay && t' <= delay+(d-1) then ON else OFF
  where t' = t `mod` td

fig12 :: IO [Weight]
fig12 = MWC.withSystemRandom (\g -> runReader (evalRandT (adapt $ untilTs (td*100)) g) env)
  where
    env = Env { mean = 0.005
              , stDev = 0.3
              , alfa = 0.9
              , beta = 0.0
              , lr = 0.2
              , signals = outSigGen:inSigGen
              }
    td = 3+3+20+10
    inSigGen = replicate 4 (signalGen 0 3 td, id)
    outSigGen = (signalGen 3 3 td, const 0.6)    
    
debug :: Show a => a -> a
debug a | trace (show a) False = undefined
debug a = a