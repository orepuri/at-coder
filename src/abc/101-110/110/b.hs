main :: IO ()
main = do
  [n, m, x, y] <- map read . words <$> getLine
  xs <- map read . words <$> getLine
  ys <- map read . words <$> getLine
  putStrLn $ if solve x y xs ys then "No War" else "War"

solve :: Int -> Int -> [Int] -> [Int] -> Bool
solve x y xs ys = any (\z -> all (<z) xs && all (>=z) ys) [x+1..y]