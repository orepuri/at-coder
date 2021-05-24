main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  print $ if b == 1 then 0 else head $ filter (\n -> a * n - (n - 1) >= b) [1..]
