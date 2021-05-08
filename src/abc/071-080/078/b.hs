main :: IO ()
main = do
  [x, y, z] <- map read . words <$> getLine
  print $ solve x y z

solve :: Double -> Double -> Double -> Int
solve x y z = floor $ (x - z) / (y + z)
