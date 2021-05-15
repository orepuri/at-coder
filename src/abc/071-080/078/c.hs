main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  print $ solve n m

solve :: Int -> Int -> Int
solve n m = t * pinv
  where
    t = 1900 * m + 100 * (n - m)
    pinv = 2 ^ m

-- 1回の計算時間 t = 1900 * m + 100 (n - m)
-- 成功する確率 p = (1/2) ^ m
-- 確率pの事象が起こるまでの試行回数の期待値は1/p
-- 計算時間の期待値 = 1回の計算時間 x 試行回数の期待値
