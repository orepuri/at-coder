main :: IO ()
main = do
  [s, w] <- map read . words <$> getLine :: IO [Int]
  putStrLn $ if w >= s then "unsafe" else "safe"