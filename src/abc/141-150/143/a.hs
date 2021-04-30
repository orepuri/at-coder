main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  print $ if 2 * b >= a then 0 else a - 2 * b