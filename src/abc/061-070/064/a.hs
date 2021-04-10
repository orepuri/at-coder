main :: IO ()
main = do
  [r, g, b] <- map read . words <$> getLine
  putStrLn $ if (r * 100 + g * 10 + b) `mod` 4 == 0 then "YES" else "NO"
