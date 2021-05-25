main :: IO ()
main = do
  [n, k, m] <- map read . words <$> getLine
  as <- map read . words <$> getLine
  print $ solve n k m as

solve :: Int -> Int -> Int -> [Int] -> Int
solve n k m as = if null scores then - 1 else fst $ head scores
  where
    total = sum as
    scores = filter (\(_, s) -> s >= n * m) $ map (\s -> (s, total + s)) [0..k]