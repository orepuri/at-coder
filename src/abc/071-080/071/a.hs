main :: IO ()
main = do
  [x, a, b] <- map read . words <$> getLine
  putStrLn $ if abs (x - a) > abs (x - b) then "B" else "A"