import Control.Monad

import qualified Data.ByteString.Char8 as C
import Data.Char

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VAI

main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine
  hs <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  mhs <- VU.thaw hs
  VAI.sortBy (flip compare) mhs
  hs' <- VU.freeze mhs
  print $ solve n k hs'

solve :: Int -> Int -> VU.Vector Int -> Int
solve n k hs = VU.sum $ VU.drop k hs
