main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  putStrLn $ if odd a && odd b then "Yes" else "No"