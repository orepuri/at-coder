{-# LANGUAGE BangPatterns #-}

import Control.Monad

import qualified Data.ByteString.Char8 as C
import Data.Char
import qualified Data.IntSet as IS

import qualified Data.Vector.Unboxed as VU

import Debug.Trace

main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve n k as

solve :: Int -> Int -> VU.Vector Int-> Int
solve n k as
  | n2 == 0 = go2 1 $ k `mod` n1
  | otherwise = go2 (go2 1 n1) $ (k - n1) `mod` n2
  where
    (n1, n2) = go 0 (IS.singleton 1) (VU.head as)
    go !count !visited !next
      | next == 1 = (count + 1, 0)
      | IS.member next visited = (count + 1, fst $ go 0 (IS.singleton next) (as VU.! (next - 1)))
      | otherwise = go (count + 1) (IS.insert next visited) (as VU.! (next - 1))
    go2 !next !count 
      | count == 0 = next
      | otherwise = go2 (as VU.! (next - 1)) (count - 1)

-- 3 2 4 1
-- 1 -> 3 -> 4 -> 1 -> 3 -> 4

-- (n1, n2) = (3, 0)

-- go2 1 3
-- go2 3 2
-- go2 4 1
-- go2 1 0