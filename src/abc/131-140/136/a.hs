main :: IO ()
main = do
  [a, b, c] <- map read . words <$> getLine
  let ans = c - (a - b)
  print $ if ans <= 0 then 0 else ans