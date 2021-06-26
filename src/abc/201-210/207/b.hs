main :: IO ()
main = do
  [a, b, c, d] <- map read . words <$> getLine
  print $ solve a b c d

solve :: Int -> Int -> Int -> Int -> Int
solve a b c d
  | null xs = -1
  | otherwise = head xs
  where
    xs = filter (\i -> a + i * b <= i * c * d) [0..10^5] 
