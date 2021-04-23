main :: IO ()
main = do
  [x, y] <- map read . words <$> getLine
  print $ x + (y `div` 2)