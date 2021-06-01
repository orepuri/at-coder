import Control.Monad

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  print $ solve n m

solve :: Int -> Int -> Int
solve n m
  | n == 1 && m == 1 = 1
  | n * m == 2 = 0
  | n == 1 || m == 1 = max n m - 2
  | otherwise = (n - 2) * (m - 2)
