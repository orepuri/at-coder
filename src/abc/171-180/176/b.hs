main :: IO ()
main = do
  n <- map read . words <$> getLine
  putStrLn $ if solve n then "Yes" else "No"

solve :: [Integer] -> Bool
solve n = sum n `mod` 9 == 0