main :: IO ()
main = do
  as <- map read . words <$> getLine :: IO [Int]
  print $ minimum as
