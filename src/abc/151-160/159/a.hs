main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine :: IO [Int]
  print $ (m * (m - 1) + n * (n - 1)) `div` 2
