main :: IO ()
main = do
  [d, n] <- map read . words <$> getLine
  print $ solve d n

solve :: Int -> Int -> Int
solve d n = cands !! (n - 1)
  where
    cands
      | d == 0 = filter (\n -> n `mod` 100 /= 0) [1..]
      | d == 1 = filter (\n -> n `mod` 10000 /= 0) [100,200..]
      | d == 2 = filter (\n -> n `mod` 100^3 /= 0) [10000,20000..]