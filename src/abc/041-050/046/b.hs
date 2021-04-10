main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine
  print $ solve n k

solve :: Int -> Int -> Int
solve n k = product $ k : replicate (n - 1) (k - 1)
