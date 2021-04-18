main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  print $ if b >= a then a else a - 1