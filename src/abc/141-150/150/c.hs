import Control.Monad
import Data.List

import Data.Maybe

main :: IO ()
main = do
  n <- readLn :: IO Int
  ps <- map read . words <$> getLine
  qs <- map read . words <$> getLine
  print $ solve n ps qs

solve :: Int -> [Int] -> [Int] -> Int
solve n ps qs = abs (pi - qi)
  where
    all = sort $ permutations [1..n]
    pi = fromJust $ elemIndex ps all
    qi = fromJust $ elemIndex qs all
