main :: IO ()
main = do
  [x, y] <- map read . words <$> getLine
  putStrLn $ if solve x y then "Yes" else "No"

solve :: Int -> Int -> Bool
solve x y = not $   null [(i, j)| i <- [0..x], let j = x - i, i * 2 + j * 4 ==y]