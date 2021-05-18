main :: IO ()
main = do
  n <- readLn
  print $ solve n

solve :: Int -> Int
solve n = length $ filter (\ds -> length ds == 8) $ map factors $ filter odd [1..n]
  where
    factors n = [x|x <- [1..n], n `mod` x == 0]