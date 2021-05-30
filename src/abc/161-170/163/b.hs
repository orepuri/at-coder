main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  as <- map read . words <$> getLine
  let days = n - sum as
  print $ if days >= 0 then days else -1