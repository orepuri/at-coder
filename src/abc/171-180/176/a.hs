main :: IO ()
main = do
  [n, x, t] <- map read . words <$> getLine
  print $ ceiling (toRational n / toRational x) * t