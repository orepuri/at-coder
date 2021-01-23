main :: IO ()
main = do
  [d, t, s] <- map read . words <$> getLine
  putStrLn $ if t * s >= d then "Yes" else "No"