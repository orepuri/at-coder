{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.List

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Debug.Trace

main :: IO ()
main = do
  [n, m, k] <- map read . words <$> getLine
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  bs <- VU.unfoldrN m (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve n m k as bs

solve :: Int -> Int -> Int -> VU.Vector Int -> VU.Vector Int -> Int
solve n m k as bs = fst $ foldl' go (0, m) [0..n] 
  where
    go (!acc, !j) !i
      | j < 0 = (acc, j)
      | k >= ta + tb = (max acc (i + j), j)
      | otherwise = go (acc, j - 1) i
      where
        !ta = tas VU.! i
        tb = tbs VU.! j
    tas = VU.scanl' (+) 0 as
    tbs = VU.scanl' (+) 0 bs
