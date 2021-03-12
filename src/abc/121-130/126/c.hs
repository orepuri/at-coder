main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine
  let a = map (solve n k) [1..n] 
  print $ sum a

solve :: Int -> Int -> Int -> Double
solve n k i = 1.0 / fromIntegral n * (0.5 ^ times 0)
  where
    times c = if i * 2 ^ c >= k then c else times (c + 1)

times' k n = ceiling $ logBase (fromIntegral n) (fromIntegral k)

-- n ^ times >= k
-- times >= logn k

-- 1 <= n <= 10^5
-- 1 <= k <= 10^5

-- n = 3, k = 10
-- n = 1, 2,4,8,10 4回表
--   1/3 * (1/2)^4
-- n = 2 , 4,8,10 3回表
--   1/3 * (1/2)^3
-- n = 3, 6,12 2回
--   1/3 * (1/2)^2


-- n = 100,000, k = 5
-- n = 1, 2,4,8 3回
-- n = 2, 4, 8 2回
-- n = 3, 6 1回
-- n = 4, 8 1回
-- n >= 5, 5 0回

-- Σ (1/n * (1/2)^x) 1 <= n <= k - 1
