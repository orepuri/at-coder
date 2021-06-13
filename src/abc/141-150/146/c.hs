

import Control.Monad
import Control.Monad.Primitive

import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VAI

import Debug.Trace

main :: IO ()
main = do
  [a, b, x] <- map read . words <$> getLine
  print $ solve a b x

solve :: Int -> Int -> Int -> Int
solve a b x
  | price 1 > x = 0
  | price maxP <= x = 10^9
  | otherwise = go 0 maxP
  where
    price n = a * n + b * length (show n)
    maxP = 10^9
    go from to
      | from >= to = n
      | price' == x = n
      | price' > x = if n <= from then n else go from (n - 1)
      | otherwise = if n >= to then n else go n to
      where
        t = from + to
        n = if even t then t `div` 2 else t `div` 2 + 1
        price' = price n
 