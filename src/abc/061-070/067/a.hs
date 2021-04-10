main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  putStrLn $ if any (\c -> c `mod` 3 == 0) [a, b, a + b] then "Possible" else "Impossible"