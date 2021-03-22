main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  print $ solve n m

solve :: Double -> Double -> Double
solve n m = min arg (360 - arg)
  where
    long = m
    short = fromIntegral (floor n `mod` 12 * 5) + m/12.0
    arg = abs (long - short) * 6
