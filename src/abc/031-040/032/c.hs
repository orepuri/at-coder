import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.Maybe

main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine
  ss <- replicateM n $ do
    fst . fromJust . C.readInt <$> C.getLine
  print $ if 0 `elem` ss then n else solve' (fromIntegral k) ss ss 1 0

solve' :: Int -> [Int] -> [Int] -> Int -> Int -> Int
solve' _ _ [] _ acc = acc
solve' k (l:ls) (r:rs) prd acc
  | r * prd <= k = solve' k (l:ls) rs (r*prd) (acc+1)
  | acc == 0 = solve' k ls rs 1 0
  | otherwise = max acc $ solve' k ls (r:rs) (prd `div` l) (acc - 1)    
    
-- 条件を満たす => rをすすめる +1
-- 条件を満たさない => これまでの結果 or lをすすめる/累積からl分を引く/-1
-- 条件を満たさない & 累積が0 => lとrをすすめて0から判定