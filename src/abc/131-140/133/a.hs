main :: IO ()
main = do
  [n, a, b] <- map read . words <$> getLine :: IO [Int]
  print $ min (n * a) b