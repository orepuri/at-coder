import Control.Monad
import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString.Char8 as C
import Data.List
import qualified Data.Set as S

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  abs <- replicateM m $ do
    [a, b] <- map read . words <$> getLine
    return (a, b)
  k <- readLn
  cds <- replicateM k $ do
    map read . words <$> getLine
  print $ solve abs cds

solve :: [(Int, Int)] -> [[Int]] -> Int
solve abs cds = maximum $ map cond cand
  where
    cand = map S.fromList $ sequence cds
    cond :: S.Set Int -> Int
    cond dishes = length $ filter (\(a, b) -> S.member a dishes && S.member b dishes) abs
