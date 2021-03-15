main :: IO ()
main = do
  [x, y] <- map read . words <$> getLine :: IO [Int]
  putStrLn $ if y > x then "Better" else "Worse"