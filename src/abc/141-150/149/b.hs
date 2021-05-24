main :: IO ()
main = do
  [a, b, k] <- map read . words <$> getLine
  putStrLn $ unwords $ map show $ solve a b k

solve :: Int -> Int -> Int -> [Int]
solve a b k = [takahashi, aoki]
  where
    takahashi = max 0 $ if a <= k then 0 else a - k
    aoki = max 0 $ if a >= k then b else b - (k - a)