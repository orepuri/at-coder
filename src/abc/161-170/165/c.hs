{-# LANGUAGE BangPatterns #-}

import Control.Monad

main :: IO ()
main = do
  [n, m, q] <- map read . words <$> getLine
  qs <- replicateM q $ do
    [a, b, c, d] <- map read . words <$> getLine
    return (a, b, c, d)
  print $ solve n m q qs

solve :: Int -> Int -> Int -> [(Int, Int, Int, Int)] -> Int
solve n m q qs = maximum $ map go $ orderedPrmutationN n m
  where
    go :: [Int] -> Int
    go as = score as
    score :: [Int] -> Int
    score as = sum $ map (\(_, _, _, d) -> d) $ filter (\(a, b, c, _) -> as !! (b - 1) - as !! (a - 1) == c) qs


orderedPrmutationN :: Int -> Int -> [[Int]]
orderedPrmutationN 1 m = map (:[]) [1..m]
orderedPrmutationN n m = [h:l|l <- orderedPrmutationN (n - 1) m, h <- [1..head l]]
