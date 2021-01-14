main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- map read . words <$> getLine
  print $ solve as

solve :: [Int] -> Int
solve as = k
  where (degree, k) = maximum $ map (\k -> (length $ filter (\a -> a `mod` k == 0) as, k)) [2..1000]
