modulus = 10^9+7

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  print $ solve n m

solve :: Int -> Int -> Int
solve n m
  | n == m = 2 `multiMod` factN `multiMod` factN
  | abs (n-m) > 1 = 0
  | n > m = n `multiMod` factM `multiMod` factM
  | otherwise = m `multiMod` factN `multiMod` factN
  where
    factN = factMod n
    factM = factMod m
    factMod 1 = 1
    factMod n = n `multiMod` factMod (n - 1)
    multiMod a b = a * b `mod` modulus