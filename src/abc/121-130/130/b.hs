main :: IO ()
main = do
  [n, x] <- map read . words <$> getLine
  ls <- map read . words <$> getLine
  print $ solve n x ls

solve :: Int -> Int -> [Int] -> Int
solve n x ls = length 
  $ takeWhile (\(d, _) -> d <= x) 
  $ take (n + 1)
  $ iterate (\(d, i) -> (d + ls !! i, i + 1)) (0, 0)