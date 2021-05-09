main :: IO ()
main = do
  n <- readLn
  xs <- map read . words <$> getLine
  print $ sum $ n : xs