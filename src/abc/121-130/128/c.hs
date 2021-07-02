import Control.Monad
import Data.Bits

import Debug.Trace

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  switchesPerLight <- replicateM m $ do
    (k:switches) <- map read . words <$> getLine
    return switches
  ps <- map read . words <$> getLine
  print $ solve n m switchesPerLight ps

solve :: Int -> Int -> [[Int]] -> [Int] -> Int
solve n m switchesPerLight ps = length $ filter id $ map allOn [0..2^n-1]
  where
    allOn :: Int -> Bool
    allOn state = all on' $ zip ps switchesPerLight
      where
        on' (p, switches) = 
          p == (length (filter (\s -> testBit state (s - 1)) switches) `mod` 2)
          
