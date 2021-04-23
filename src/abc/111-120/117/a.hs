main :: IO ()
main = do
  [t, x] <- map read . words <$> getLine
  print $ t / x