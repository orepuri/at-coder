main :: IO ()
main = do
  [h1, w1] <- map read . words <$> getLine :: IO [Int]
  [h2, w2] <- map read . words <$> getLine
  putStrLn $ if h1 == h2 || h1 == w2 || h2 == w1 || w1 == w2 then "YES" else "NO"
