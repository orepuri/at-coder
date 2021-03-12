main :: IO ()
main = do
  [a, d] <- map read . words <$> getLine
  print $ solve a d

solve :: Int -> Int -> Int
solve a d
  | a > d = a * (d + 1)
  | otherwise = (a + 1) * d
