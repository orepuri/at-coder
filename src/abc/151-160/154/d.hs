import Control.Monad
import Control.Monad.Primitive

import qualified Data.ByteString.Char8 as C
import Data.Char

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.List
import Debug.Trace

main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine
  ps <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  let es = VU.map (\p -> (1 + fromIntegral p) / 2) ps :: VU.Vector Double
  as <- VU.thaw es -- VUM.replicate n 0 :: IO (VUM.MVector (PrimState IO) Int)
  forM_ [1..n-1] $ \i -> do
    p <- VUM.read as (i - 1)
    VUM.modify as (+p) i
  as' <- VU.freeze as    
  let ans = if n == k then as' VU.! (n-1) else maximum $ map (\i -> as' VU.! (i+k) - as' VU.! i) [0..n-k-1]
  print ans


solve :: Int -> Int -> VU.Vector Int -> Double
solve n k ps = go base base 1
  where
    go acc prev i
      | i >= n - k + 1 = acc
      | otherwise = go (max acc prev') prev' (i+1) 
      where
        prev' = prev - ev (i-1) + ev (i + k - 1)
    base = VU.sum $ VU.map (es VU.!) $ VU.take k ps
    ev i = es VU.! (ps VU.! i)
    -- es[p] .. pの期待値
    es :: VU.Vector Double
    es = VU.map (\(_, _, e) -> e) $
      VU.scanl' (\(_, acc, p) i -> (i, acc + i, fromIntegral (acc + i) / fromIntegral i))
      (0, 0, 0) $
      VU.generate 1000 (+1)
