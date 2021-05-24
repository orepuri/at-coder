main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine :: IO [Int]
  hs <- map read . words <$> getLine
  print $ length $ filter (>=k) hs