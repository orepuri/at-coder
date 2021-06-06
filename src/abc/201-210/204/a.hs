main :: IO ()
main = do
  [x, y] <- map read . words <$> getLine
  print $ if x == y then x else head $ filter (\i -> i /= x && i /= y) [0,1,2]
