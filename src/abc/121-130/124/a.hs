main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  print $ if a == b then a + b else if a > b then 2 * a - 1 else 2 * b - 1