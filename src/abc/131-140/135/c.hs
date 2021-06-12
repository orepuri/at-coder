{-# LANGUAGE BangPatterns #-}

import Control.Monad

import qualified Data.ByteString.Char8 as C
import Data.Char


import Data.Maybe
import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldrN (n+1) (C.readInt . C.dropWhile isSpace) <$> C.getLine
  bs <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve n as bs

solve :: Int -> VU.Vector Int -> VU.Vector Int -> Int
solve n as bs = fst $ VU.ifoldl' go (0, 0) as
  where
    go (!acc, !hiros) !i !a = (acc + min a hiros', min current (max 0 (hiros' - a)))
      where
        current = bs VU.! i
        !hiros' = if i < n then hiros + current else hiros

