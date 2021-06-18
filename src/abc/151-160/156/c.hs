main :: IO ()
main = do
  n <- readLn :: IO Int
  xs <- map read . words <$> getLine
  print $ solve n xs

solve :: Int -> [Int] -> Int
solve n xs = minimum $ map powers [minX..maxX]
  where
    maxX = maximum xs
    minX = minimum xs
    powers p = sum $ map (\x -> (x-p)^2) xs