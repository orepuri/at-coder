main :: IO ()
main = do
  [a, b, x] <- map read . words <$> getLine
  putStrLn $ if a <= x && a + b >= x then "YES" else "NO"