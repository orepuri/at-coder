import Data.List

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  print $ solve n m

solve :: Int -> Int -> Int
solve n m
  | m < 2 * n = m `div` 2
  | otherwise = n + (m - 2 * n) `div` 4
