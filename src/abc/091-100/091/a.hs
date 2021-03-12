main :: IO ()
main = do
  [a, b, c] <- map read . words <$> getLine
  putStrLn $ if a + b >= c then "Yes" else "No"
