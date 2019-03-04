module Main where

import AssociativeSearchNetwork
import Text.Printf

main :: IO ()
main = do
  ls <- fig4
  writeFile "/home/vpo/workspace/associative-search-network/out.csv" $ foldl (\s t -> s ++ show (fst t) ++ "," ++ printf "%.4f" (snd t) ++ "\r\n") "Time Stamp,Return\r\n" ls