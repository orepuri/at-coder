import Control.Monad
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
  s <- C.getLine
  print $ solve s

solve :: C.ByteString -> Int
solve s = min n1 n2
  where
    n1 = foldl' (go '0' ) 0 si
    n2 = foldl' (go '1' ) 0 si
    si = zip (C.unpack s) [1..]
    go z acc (c, i)
      | odd i = if z == c then acc else acc + 1
      | otherwise = if z == c then acc + 1 else acc

