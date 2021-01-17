main :: IO ()
main = do
  [n, a, b] <- map read . words <$> getLine
  print $ solve n a b

solve :: Int -> Int -> Int -> Int
solve n a b = n - a + b
