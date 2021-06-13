{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -O2 #-}

import Control.Monad

import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
  n <- readLn :: IO Int
  s <- C.getLine
  print $ solve n s

solve :: Int -> C.ByteString  -> Int
solve n s = fst $ C.foldl' go (1, C.head s) (C.tail s)
  where
    go (!acc, !pc) !c
      | pc == c = (acc, c)
      | otherwise = (acc + 1, c)
