import Data.List

main :: IO ()
main = do
  n <- readLn
  as <- map read . words <$> getLine
  print $ solve n as

solve :: Int -> [Int] -> Int
solve n (a:as)
  | people `mod` n /= 0 = -1
  | length (nub (a:as)) == 1 = 0
  | otherwise = snd $ foldl' (\(pa, bridges) a -> if pa == avg then (a, bridges) else (if pa > avg then a + pa - avg else a - (avg - pa), bridges + 1)) (a, 0) as
  where
    people = sum (a:as)
    avg = people `div` n
