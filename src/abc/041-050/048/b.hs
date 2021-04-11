main :: IO ()
main = do
  [a, b, x] <- map read . words <$> getLine :: IO [Int]
  print $ f b x - f (a - 1) x
  where
    f (-1) x = 0
    f n x = n `div` x + 1
