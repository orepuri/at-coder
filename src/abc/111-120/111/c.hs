{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString.Char8 as C
import Data.Char
import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  n <- readLn :: IO Int
  vs <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve n vs

solve :: Int -> VU.Vector Int -> Int
solve n vs
  | ov1 /= ev1 = n - oc1 - ec1
  | otherwise = min (n - oc2 - ec1) (n - oc1 - ec2)
  where
    indexed = VU.indexed vs
    odds = VU.filter (\(i, _) -> odd i) indexed
    evens = VU.filter (\(i, _) -> even i) indexed
    oddFs = VU.accumulate (+) (VU.replicate (10^5+1) 0) $ VU.map (\(_, v) -> (v, 1)) odds
    evenFs = VU.accumulate (+) (VU.replicate (10^5+1) 0) $ VU.map (\(_, v) -> (v, 1)) evens
    (oc1, ov1) = (VU.maximum oddFs, VU.maxIndex oddFs)
    (ec1, ev1) = (VU.maximum evenFs, VU.maxIndex evenFs)
    oc2 = VU.maximum $ VU.ifilter (\i _ -> i /=ov1) oddFs
    ec2 = VU.maximum $ VU.ifilter (\i _ -> i /=ev1) evenFs
