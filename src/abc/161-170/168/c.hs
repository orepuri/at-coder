main :: IO ()
main = do
  [a, b, h, m] <- map read . words <$> getLine
  print $ solve a b h m

solve :: Double -> Double -> Double -> Double -> Double
solve a b h m = sqrt $ a^2 + b^2 - 2 * a * b * cos (pi / 180 * arcX)   
  where
    arcH = 30 * h + m / 2
    arcM = 6 * m
    arcX = max (arcH - arcM) (arcM - arcH)