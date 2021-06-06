main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- map read . words <$> getLine
  print $ solve n as

solve :: Int -> [Int] -> Int
solve n as = sum $ map (\a -> a - 10) $ filter (> 10) as
