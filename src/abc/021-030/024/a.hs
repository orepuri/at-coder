main :: IO ()
main = do
  [a, b, c, k] <- map read . words <$> getLine
  [s, t] <- map read . words <$> getLine
  let total = a * s + b * t
      discount = (s + t) * c
  print $ if s + t >= k then total - discount else total
