main :: IO ()
main = do
  [a, b, c, d] <- map read . words <$> getLine
  let t = min b d - max a c
  print $ if t >= 0 then t else 0