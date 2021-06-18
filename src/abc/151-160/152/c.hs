{-# LANGUAGE BangPatterns #-}

import Control.Monad

import qualified Data.ByteString.Char8 as C
import Data.Char

import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  n <- readLn :: IO Int
  ps <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve n ps

solve :: Int -> VU.Vector Int-> Int
solve n ps = fst $ VU.foldl' go (0, maxBound) ps 
  where
    go (!acc, !pp) !p
      | pp > p = (acc+1, p)
      | otherwise = (acc, pp)

