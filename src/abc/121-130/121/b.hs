import Control.Monad

main :: IO ()
main = do
  [n, m, c] <- map read . words <$> getLine
  bs <- map read . words <$> getLine
  as <- replicateM n $ do
    map read . words <$> getLine
  print $ solve as bs c

solve :: [[Int]] -> [Int] -> Int -> Int
solve as bs c = length $ filter (\a -> c + sum (zipWith (*) a bs) > 0) as
