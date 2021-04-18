main :: IO ()
main = do
  [a, b, c, d] <- map read . words <$> getLine
  putStrLn $ if abs (a - c) <= d || (abs (a - b) <= d && abs (b - c) <= d) then "Yes" else "No"