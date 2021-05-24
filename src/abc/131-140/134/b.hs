main :: IO ()
main = do
  [n, d] <- map read . words <$> getLine
  print $ solve n d

solve :: Int -> Int -> Int
solve n d = ceiling $ fromIntegral n / fromIntegral (2 * d + 1)
