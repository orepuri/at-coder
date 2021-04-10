main :: IO ()
main = do
  [x, a, b] <- map read . words <$> getLine
  putStrLn $ solve x a b

solve :: Int -> Int -> Int -> String
solve x a b
  | a - b >= 0 = "delicious"
  | b - a < x + 1 = "safe"
  | otherwise = "dangerous"
