import Control.Monad

main :: IO ()
main = do
  [n, d] <- map read . words <$> getLine :: IO [Int]
  xxs <- replicateM n $ do
    map read . words <$> getLine
  print $ solve n xxs

solve :: Int -> [[Int]] -> Int
solve n xxs = length $ filter isSquare $ map dist ijs
  where
    ijs = [(i, j)| i <- [0..n-2], j <- [1..n-1], i < j]
    dist (i, j) = sum $ zipWith (\xi xj -> (xi - xj)^2) (xxs !! i) (xxs !! j)
    isSquare d = any (\i -> d == i * i) [0..d-1]