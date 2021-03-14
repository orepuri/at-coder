main :: IO ()
main = do
  [m, h] <- map read . words <$> getLine
  putStrLn $ if h `mod` m == 0 then "Yes" else "No"