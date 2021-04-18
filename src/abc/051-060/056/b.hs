main :: IO ()
main = do
  [w, a, b] <- map read . words <$> getLine
  let move = if a < b then b - (a + w) else a - (b + w)
  print $ if move < 0 then 0 else move