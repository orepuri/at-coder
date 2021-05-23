main :: IO ()
main = do
  [a, b, c] <- map read . words <$> getLine
  print $ sum $ map (7 -) [a, b, c]