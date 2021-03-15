main :: IO ()
main = do
  [a, b, c] <- map read . words <$> getLine
  print $ solve a b c

solve :: Int -> Int -> Int -> Int
solve a b c = if a > b
  then y + (c - b * y) `div` a
  else x + (c - a * x) `div` b
  where
    x = c `div` a
    y = c `div` b