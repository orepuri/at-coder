main :: IO ()
main = do
  [a, b, c] <- map read . words <$> getLine
  print $ if b >= a * c then c else b `div` a