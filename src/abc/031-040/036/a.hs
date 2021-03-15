main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  print $ ceiling (b / a)