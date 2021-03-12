main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  print $ solve a b

solve :: Int -> Int -> Int
solve a b = 2 * a + 100 - b