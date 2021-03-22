main :: IO ()
main = do
  [a, b, c, d] <- map read . words <$> getLine
  print $ if a * b > c * d then a * b else c * d