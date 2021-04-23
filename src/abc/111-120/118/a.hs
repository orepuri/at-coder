main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  print $ if b `mod` a == 0 then a + b else b - a