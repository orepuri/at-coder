main :: IO ()
main = do
  [p, q, r] <- map read . words <$> getLine
  print $ minimum [p + q, q + r, r + p, 2 * p + r, 2 * q + p, 2 * r + q]
