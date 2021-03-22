import Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import Data.Maybe

main :: IO ()
main = do
  [n, d, k] <- map read . words <$> getLine
  lrs <- VU.replicateM d $ do
    [l, r] <- map (fst . fromJust . C.readInt) . C.words <$> C.getLine
    return (l, r)
  sts <- VU.replicateM k $ do
    [s, t] <- map (fst . fromJust . C.readInt) . C.words <$> C.getLine
    return (s, t)
  let ans = solve lrs sts
  VU.forM_ ans print

solve :: VU.Vector (Int, Int) -> VU.Vector (Int, Int) -> VU.Vector Int
solve lrs = VU.map f
  where
    f (s, t) = go s t (s,s) lrs 0
    go s t (from, to) lrs days
      | s < t && from <= s && t <= to = days
      | t < s && from <= t && s <= to = days
      | otherwise = go s t (newRange (from,to) lr) (VU.tail lrs) (days + 1)
      where
        lr = VU.head lrs
newRange (f1,t1) (f2,t2)
  | t1 < f2 || t2 < f1 = (f1, t1)
  | otherwise = (min f1 f2, max t1 t2)
