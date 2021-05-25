main :: IO ()
main = do
  [n, a, b] <- map read . words <$> getLine
  print $ solve n a b

solve :: Int -> Int -> Int -> Int
solve n blues reds = times * blues + min remains blues
  where
    times = n `div` (blues + reds)
    remains = n - (blues + reds) * times
    
