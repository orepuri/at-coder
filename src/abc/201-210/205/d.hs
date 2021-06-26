{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString.Char8 as C
import Data.Char

import Data.Maybe
import qualified Data.Vector.Unboxed as VU

import Debug.Trace

main :: IO ()
main = do
  [n, q] <- map read . words <$> getLine
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  ks <- VU.replicateM q $ fst . fromJust . C.readInt <$> C.getLine
  VU.forM_ (solve n as ks) print

solve :: Int -> VU.Vector Int -> VU.Vector Int -> VU.Vector Int
solve n as = VU.map go
  where
    cs = VU.scanl1' (+) $ VU.zipWith ((-) . subtract 1) as $ VU.cons 0 as
    go !k
      | cn < k = VU.last as + (k - cn)
      | otherwise = if i == 0 then k else (as VU.! i - 1) - (cs VU.! i - k)
      where
        i = binarySearch (>=k) 0 n cs
    !cn = VU.last cs

binarySearch :: (Int -> Bool) -> Int -> Int -> VU.Vector Int -> Int
binarySearch !p !l !r !xs
  | l >= r = l
  | p xm = binarySearch p l m xs
  | otherwise = binarySearch p (m + 1) l xs
  where
    !m = (l + r) `div` 2
    xm = xs VU.! m


-- k = 2 5 3
-- a = 3 5 6 7
-- c = 2 3 3 3
-- cn=3
-- ans = 2 9 4

-- binarySearch (>=3) 0 4 [2,3,3,3]
-- m = 2, xm = 3, p 3 = True
-- binarySearch (>=3) 0 2 [2,3,3,3]
-- m = 1, xm = 3, p 3 = True
-- binarySearch (>=3) 0 1 [2,3,3,3]
-- m = 0, xm = 2, p 2 = False
-- binarySearch (>=3) 1 1 [2,3,3,3]
-- 1
