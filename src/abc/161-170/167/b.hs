main :: IO ()
main = do
  [a, b, c, k] <- map read . words <$> getLine
  print $ solve a b c k

solve :: Int -> Int -> Int -> Int -> Int
solve a b c k
  | k <= a = k
  | k <= a + b = a
  | otherwise = a - (k-a-b)