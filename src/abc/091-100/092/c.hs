import Control.Monad
import qualified Data.Array.IO as IO
import qualified Data.Array.Unboxed as AU
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.List
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  VU.forM_ (solve n as) print

solve :: Int -> VU.Vector Int -> VU.Vector Int
solve n as = VU.map go $ VU.generate n id
  where
    go i
      | i == 0 = total - d 0 a an
      | i == n - 1 = total - d ap a 0
      | otherwise = total - d ap a an
      where
        a = as VU.! i
        an = as VU.! (i + 1)
        ap = as VU.! (i - 1)        
        d ap a an
          | b1 <= a && a <= b2 = 0
          | a < b1 = 2 * abs (a - b1)
          | otherwise = 2 * abs (a - b2)
          where
            b1 = min ap an
            b2 = max ap an
    pairs = VU.zip as (VU.tail as)
    total = abs (as VU.! (n-1)) + VU.foldl' (\acc (a1, a2) -> acc + abs (a1 - a2)) (abs $ VU.head as) pairs
