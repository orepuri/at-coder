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

main :: IO ()
main = do
  h <- readLn :: IO Int
  print $ solve h

solve :: Int -> Int
solve h = go 0 [h]
  where
    go acc [] = acc
    go acc (h:hs)
      | h == 1 = go (acc + 1) hs
      | otherwise = go (acc + 1) (h': h':hs)
      where
        h' = h `div` 2
