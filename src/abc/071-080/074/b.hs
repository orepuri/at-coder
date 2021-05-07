main :: IO ()
main = do
  n <- readLn
  k <- readLn
  xs <- map read . words <$> getLine
  print $ solve n k xs

solve :: Int -> Int -> [Int] -> Int
solve n k xs = sum $ map (\x ->  2 * min x (abs (k - x)) ) xs