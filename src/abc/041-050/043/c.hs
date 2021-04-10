main :: IO ()
main = do
  n <- readLn
  as <- map read . words <$> getLine
  print $ solve n as

solve :: Int -> [Int] -> Int
solve n as = min (sum $ map go as) (sum $ map go' as)
  where
    avg = ceiling $ fromIntegral (sum as) / fromIntegral n
    avg' = floor $ fromIntegral (sum as) / fromIntegral n
    go a = (avg - a) ^ 2
    go' a = (avg' - a) ^ 2