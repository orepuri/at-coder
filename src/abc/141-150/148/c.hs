main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  print $ lcm a b
