main :: IO ()
main = do
  n <- readLn :: IO Int
  vs <- map read . words <$> getLine
  cs <- map read . words <$> getLine
  print $ solve vs cs

solve :: [Int] -> [Int] -> Int
solve vs cs = sum $ filter (>0) $ zipWith (-) vs cs