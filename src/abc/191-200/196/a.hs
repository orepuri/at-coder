main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  [c, d] <- map read . words <$> getLine
  print $ b - c
