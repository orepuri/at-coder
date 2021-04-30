main :: IO ()
main = do
  [a, b, c] <- map read . words <$> getLine :: IO [Int]
  putStrLn $ if (a == b && a /= c) || (b == c && b /= a) || (c == a && c /= b) then "Yes" else "No"