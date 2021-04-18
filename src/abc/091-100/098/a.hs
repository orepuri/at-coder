main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  print $ maximum [a + b, a - b, a * b]