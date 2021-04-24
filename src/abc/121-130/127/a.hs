main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  print $ if a <= 5 then 0 else if a <= 12 then b `div` 2 else b