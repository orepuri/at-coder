import Control.Monad

main :: IO ()
main = do
  n <- readLn
  bs <- replicateM (n-1) $ do
    read <$> getLine
  print $ solve n bs

solve :: Int -> [Int] -> Int
solve n bs = salary 1
  where
    subordinate i = map snd $ filter (\(p, _) -> p==i) $ zip bs [2..]
    salary :: Int -> Int
    salary i
      | null subordinate' = 1
      | length subordinate' == 1 = 1 + 2 * salary (head subordinate')
      | otherwise = maximum (map salary subordinate') + minimum (map salary subordinate') + 1
      where
        subordinate' :: [Int]
        subordinate' = subordinate i