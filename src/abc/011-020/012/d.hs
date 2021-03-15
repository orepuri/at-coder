import Control.Monad
import Data.Array.Unboxed
import Data.Array.ST
import qualified Data.ByteString.Char8 as C
import Data.Char
import qualified Data.Vector.Unboxed as VU
import Data.Maybe

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  routes <- fmap (VU.fromList . concat) $ replicateM m $ do
    [a, b, t] <- map (fst . fromJust . C.readInt) . C.words <$> C.getLine
    return [(a, b, t), (b, a, t)]
  print $ solve n routes

solve :: Int -> VU.Vector (Int, Int, Int) -> Int
solve n routes = minimum [maximum [d ! (i, j) | j <- [1..n]] | i <- [1..n]]
   where
     d = warshall n routes

inf :: Int
inf = maxBound

warshall :: Int -> VU.Vector (Int, Int, Int) -> UArray (Int, Int) Int
warshall n routes = runSTUArray $ do
  d <- newArray ((1, 1), (n, n)) (10^6 :: Int)
  let vec = VU.generate n (+1)
  VU.forM_ vec $ \i -> do
    writeArray d (i, i) 0
  VU.forM_ routes $ \(f, t, c) -> do
    writeArray d (f, t) c
  VU.forM_ vec $ \k -> do
    VU.forM_ vec $ \i -> do
      VU.forM_ vec $ \j -> do
        dik <- readArray d (i, k)
        dkj <- readArray d (k, j)
        dij <- readArray d (i, j)
        writeArray d (i, j) (min dij (dik + dkj))
  return d
