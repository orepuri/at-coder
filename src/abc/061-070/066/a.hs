main :: IO ()
main = do
  [a, b, c] <- map read . words <$> getLine
  print $ minimum [a+b, b+c, c+a]

