main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  print $ if a <= 9 && b <= 9 then a * b else -1