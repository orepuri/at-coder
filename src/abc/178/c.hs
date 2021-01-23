main :: IO ()
main = do
  n <- readLn
  print $ solve n

solve :: Int -> Int
solve n = (modPow 10 n - modPow 9 n * 2 + modPow 8 n) `mod` modulus

modPow :: Int -> Int -> Int
modPow a e
  | e == 0 = 1
  | odd e = p * p `mod` modulus * a `mod` modulus
  | otherwise = p * p `mod` modulus
  where
    p = modPow a (e `div` 2)

modulus = 10^9+7
