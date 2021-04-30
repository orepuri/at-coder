main :: IO ()
main = do
  [h, a] <- map read . words <$> getLine :: IO [Int]
  print $ if h `mod` a == 0 then h `div` a else h `div` a + 1