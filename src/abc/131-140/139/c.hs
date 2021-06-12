import Control.Monad

import qualified Data.ByteString.Char8 as C
import Data.Char

import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  n <- readLn :: IO Int
  hs <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve n hs

solve :: Int -> VU.Vector Int -> Int
solve n hs = go 0 0 (VU.head hs) (VU.tail hs)
  where
    go acc cs h hs
      | VU.null hs = max acc cs
      | h >= h' = go acc (cs + 1) h' hs'
      | otherwise = go (max acc cs) 0 h' hs'
      where
        h' = VU.head hs
        hs' = VU.tail hs
