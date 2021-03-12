main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  print $ solve a b

solve a b
  | all >= 15 && b >= 8 = 1
  | all >= 10 && b >= 3 = 2
  | all >= 3 = 3
  | otherwise = 4
  where
    all = a + b
