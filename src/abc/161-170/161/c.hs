main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine
  print $ solve n k

solve :: Int -> Int -> Int
solve n k = min t (k - t)
  where
    t = n `mod` k

