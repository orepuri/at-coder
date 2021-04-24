main :: IO ()
main = do
  [x, a] <- map read . words <$> getLine :: IO [Int]
  print $ if x < a then 0 else 10