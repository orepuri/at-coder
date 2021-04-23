main :: IO ()
main = do
  [a, b, c] <- map read . words <$> getLine
  print $ a * b `div` 2