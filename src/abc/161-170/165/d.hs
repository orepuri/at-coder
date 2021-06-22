main :: IO ()
main = do
  [a, b, n] <- map read . words <$> getLine
  print $ solve a b n

solve :: Int -> Int -> Int -> Int
solve a b n = f $ min (b - 1) n
  where
    f x = floor (fromIntegral (a * x) / fromIntegral b) - a * floor (fromIntegral x / fromIntegral b)
  