import Data.List

m = 10 ^ 9 + 7

main :: IO ()
main = do
  n <- readLn
  print $ solve n

solve :: Integer -> Integer
solve n = foldl' (\acc i -> acc * i `mod` m) 1 $ map (succ . primeFactors n') $ primesTo n
  where
    n' = factorial n

primeFactors :: Integer -> Integer -> Integer
primeFactors n i = go n i 0
  where
    go n i acc
      | n `mod` i == 0 = go (n `div` i) i (acc + 1)
      | otherwise = acc

primesTo :: Integer -> [Integer]      
primesTo n
  | n < 2 = []
  | n == 2 = [2]
  | otherwise = sieve [2..n] n
  where
    sieve [] _ = []
    sieve (x:xs) n
      | x^2 > n = x:xs
      | otherwise = x : sieve [a | a <- xs, a `mod` x > 0] n

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)
