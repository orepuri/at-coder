main :: IO ()
main = do
  [a, b, c, d] <- map read . words <$> getLine
  print $ solve a b c d

solve :: Int -> Int -> Int -> Int -> Int
solve a b c d = maximum [x1, x2, x3, x4]
  where
    x1 = a * c
    x2 = a * d
    x3 = b * c
    x4 = b * d