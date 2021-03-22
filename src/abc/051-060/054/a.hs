main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  putStr $ solve a b

solve :: Int -> Int -> String
solve a b
  | a == b = "Draw"
  | a == 1 = "Alice"
  | b == 1 = "Bob"
  | a > b = "Alice"
  | otherwise = "Bob"