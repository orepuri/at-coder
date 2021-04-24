main :: IO ()
main = do
  [a, p] <- map read . words <$> getLine
  print $ (a * 3 + p) `div` 2