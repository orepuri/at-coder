import Control.Monad
import Control.Monad.Primitive

import qualified Data.Array.IO as AIO
import qualified Data.Array.Unboxed as AU
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.List
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VAI

import Data.Array

main :: IO ()
main = do
  n <- readLn :: IO Int
  ds <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve n ds

solve :: Int -> VU.Vector Int -> Int
solve n ds = VU.length $ VU.takeWhile (==half) $ VU.dropWhile (< half) $ VU.scanl' (+) 0 freqs
  where
    half = n `div` 2
    freqs :: VU.Vector Int
    freqs = VU.accumulate (+) (VU.replicate (10^5+1) 0) $ VU.zip ds $ VU.replicate n 1
