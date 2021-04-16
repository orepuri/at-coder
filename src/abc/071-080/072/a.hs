main :: IO ()
main = do
  [x, t] <- map read . words <$> getLine
  print $ if x >= t then x - t else 0