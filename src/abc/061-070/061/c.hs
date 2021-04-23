import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Algorithms.Intro as Intro
import Data.Char
import Data.Maybe

main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine
  abs <- VU.replicateM n $ do
    [a, b] <- map (fst . fromJust . C.readInt) . C.words <$> C.getLine
    return (a, b)
  abs' <- VU.thaw abs
  Intro.sort abs'
  abs'' <- VU.freeze abs'
  print $ solve k abs''

solve :: Int -> VU.Vector (Int, Int) -> Int
solve k abs = go 0 (VU.head abs) (VU.tail abs)
  where
    go acc (a, b) abs
      | acc + b >= k = a
      | otherwise = go (acc + b) (VU.head abs) (VU.tail abs)

-- 1 2 2 3 3 3
-- (1,1) (2,2) (3,3)
-- 小さい順にカウントする