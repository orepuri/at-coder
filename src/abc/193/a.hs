main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  print $ solve a b

solve :: Double -> Double -> Double
solve a b = (1.0 - b / a) * 100
