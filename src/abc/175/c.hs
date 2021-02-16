main :: IO ()
main = do
  [x, k, d] <- map read . words <$> getLine
  print $ solve (abs x) k d

solve :: Integer -> Integer -> Integer -> Integer
solve x k d
  | even remains = m
  | otherwise = abs $ d - m
  where
    steps = min k $ x `div` d
    remains = k - steps
    m = x - d * steps
