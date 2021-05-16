main :: IO ()
main = do
  [n, m, x] <- map read . words <$> getLine
  as <- map read . words <$> getLine
  print $ solve n x as

solve :: Int -> Int -> [Int] -> Int
solve n x as = min (length r1) (length r2)
  where
    (r1, r2) = span (<x) as