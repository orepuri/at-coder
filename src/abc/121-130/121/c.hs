import Control.Monad
import qualified Data.ByteString.Char8 as C

import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VAI

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  abs <- VU.replicateM n $ do
    [a, b] <- map (fst . fromJust . C.readInt) . C.words <$> C.getLine
    return (a, b)
  abs' <- VU.thaw abs
  VAI.sortBy (\(a1, _) (a2, _) -> compare a1 a2) abs'
  print . solve n m =<< VU.freeze abs'

solve :: Int -> Int -> VU.Vector (Int, Int) -> Int
solve n m abs = go 0 m abs
  where
    go acc m abs
      | b >= m = acc + m * a
      | otherwise = go (acc + b * a) (m - b) abs'
      where
        (a, b) = VU.head abs
        abs' = VU.tail abs
