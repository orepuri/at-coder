import Data.List

main :: IO ()
main = do
  n <- readLn
  k <- readLn
  print $ solve n k

solve :: Int -> Int -> Int
solve n k = foldl' (\acc _ -> min (acc * 2) (acc + k)) 1 [1..n]