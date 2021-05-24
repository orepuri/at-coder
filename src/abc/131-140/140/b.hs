import Data.List

main :: IO ()
main = do
  n <- readLn
  as <- map read . words <$> getLine
  bs <- map read . words <$> getLine
  cs <- map read . words <$> getLine
  print $ solve n as bs cs

solve :: Int -> [Int] -> [Int] -> [Int] -> Int
solve n as bs cs = satis + sum (map (\a -> bs !! (a-1)) as)
  where
    satis = sum $ zipWith (\a pa -> if a - pa == 1 then cs !! (pa - 1) else 0) (tail as) as
