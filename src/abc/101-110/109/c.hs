import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.Char

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VAI

main :: IO ()
main = do
  [n, x] <- map read . words <$> getLine
  xs <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  xs' <- VU.thaw xs
  VAI.sort xs'
  print . solve n x =<< VU.freeze xs'

solve :: Int -> Int -> VU.Vector Int -> Int
solve n x xs = head $ filter (\i -> VU.all (\d -> d `mod` i == 0) diffs) $ factors minDiff
  where
    (l, r) = VU.span (<= x) xs
    diffA = abs (x - VU.head xs)
    diffB = abs (x - VU.last xs)
    diffs = 
        let lp = VU.zipWith (\a b -> abs (a - b)) (VU.cons x l) l
            rp = VU.zipWith (\a b -> abs (a - b)) (VU.cons x r) r
        in lp VU.++ rp
    minDiff = VU.minimum diffs
    

factors :: Int -> [Int]
factors n = [x | x <- [n,n-1..1], n `mod` x == 0]
