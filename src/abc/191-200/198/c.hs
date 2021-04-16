main :: IO ()
main = do
  [r,x,y] <- map read . words <$> getLine :: IO [Double]
  print $ solve r x y

solve :: Double -> Double -> Double -> Int
solve r x y = if r > d then 2 else ceiling $ d / r
  where
    d = sqrt (x ^ 2 + y ^ 2)
  

--- 