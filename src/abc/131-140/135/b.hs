import Data.List

main :: IO ()
main = do
  n <- readLn
  ps <- map read . words <$> getLine
  putStrLn $ if solve n ps then "YES" else "NO"

solve :: Int -> [Int] -> Bool
solve n ps = ps == sorted || any (\(i, j) -> swap i j == sorted) ijs
  where
    ijs = [(i, j)| i <- [0..n-2], j <- [1..n-1], i < j]
    sorted = sort ps
    swap i j = map (\(p, n) -> if n == i then ps !! j else if n == j then ps !! i else p) $ zip ps [0..]



