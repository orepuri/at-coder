main :: IO ()
main = do
  [x, y] <- map read . words <$> getLine
  putStrLn $ if solve x y then "Yes" else "No"

solve :: Int -> Int -> Bool
solve x y
  | x == 2 && y == 2 = True
  | all (`elem` [4, 6, 9, 11]) [x, y] = True
  | all (`elem` [1, 3, 5, 7, 8, 10, 12]) [x, y] = True
  | otherwise = False
