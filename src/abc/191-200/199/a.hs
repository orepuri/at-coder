main :: IO ()
main = do
  [a, b, c] <- map read . words <$> getLine
  putStrLn $ if a ^ 2 + b ^ 2 < c ^ 2 then "Yes" else "No"