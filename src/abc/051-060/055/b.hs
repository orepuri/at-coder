import Data.List

m :: Int
m = 10^9+7

main :: IO ()
main = do
  n <- readLn
  print $ solve n

solve :: Int -> Int
solve n = foldl' (\acc i -> i * acc `mod` m) 1 [2..n]
