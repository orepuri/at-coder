import Data.List

main :: IO ()
main = do
  [n, l] <- map read . words <$> getLine
  print $ solve n l

solve :: Int -> Int -> Int
solve n l = sum $ tail $ sortBy (\t1 t2 -> compare (abs t1) (abs t2)) $ map (\t -> l + t - 1) [1..n]
