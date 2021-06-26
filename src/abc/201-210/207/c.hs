{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString.Char8 as C

import qualified Data.Vector.Unboxed as VU
import Data.Maybe
import Debug.Trace


main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- VU.replicateM n $ do
    [t, l, r] <- map (fst . fromJust . C.readInt) . C.words <$> C.getLine
    return (t, l, r)
  print $ solve n ss

solve :: Int -> VU.Vector (Int, Int, Int) -> Int
solve n ss = length $ filter override [(i, j)| i <- [0..n-2], j <- [i+1..n-1]]
  where
    override (!i, !j) = sl == ll && sr == lr || test smaller larger
      where
        test (st, sl, sr) (lt, ll, lr) = case (st, lt) of
          (1, 1) -> sr >= ll
          (1, 2) -> sr >= ll
          (3, 1) -> sr >= ll
          (3, 2) -> sr >= ll
          _      -> sr > ll
        si @ (st, sl, sr) = ss VU.! i
        sj @ (lt, ll, lr) = ss VU.! j
        (smaller, larger) = if sl < ll
          then (si, sj)
          else if ll > sl
            then (sj, si)
            else if sr < lr
              then (si, sj)
              else (sj, si)
        
