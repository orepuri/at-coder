import Control.Monad

main :: IO ()
main = do
  [n, m, x] <- map read . words <$> getLine
  books <- replicateM n $ do
    (c:as) <- map read . words <$> getLine
    return (c, as)
  print $ solve m x books

solve :: Int -> Int -> [(Int, [Int])] -> Int
solve m x books = if ans == maxBound then -1 else ans
  where
    ans = minimum prices
    ps = powerset books
    prices = map price ps
    price :: [(Int, [Int])] -> Int
    price books
      | ok = sum $ map fst books
      | otherwise = maxBound
      where
        ok = all (\i -> x <= sum (map (\(_, as) -> as !! i) books)) [0..m-1]

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs