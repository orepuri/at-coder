main :: IO ()
main = do
  [x, y] <- map read . words <$> getLine
  print $ solve x y

solve :: Int -> Int -> Int
solve x y = length $ takeWhile (<=y) $ iterate (*2) x