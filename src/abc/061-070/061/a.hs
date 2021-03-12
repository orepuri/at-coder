main :: IO ()
main = do
  [a, b, c] <- map read . words <$> getLine :: IO [Int]
  putStrLn $ if a <= c && c <= b then "Yes" else "No"
