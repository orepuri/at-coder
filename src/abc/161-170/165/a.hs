main :: IO ()
main = do
  k <- readLn
  [a, b] <- map read . words <$> getLine
  putStrLn $ if any (\c -> c `mod` k == 0) [a..b] then "OK" else "NG"