main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  print $ (n - 1) * (m - 1)
