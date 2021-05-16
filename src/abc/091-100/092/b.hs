import Control.Monad

main :: IO ()
main = do
  n <- readLn
  [d, x] <- map read . words <$> getLine
  as <- replicateM n readLn
  print $ solve d x as

solve :: Int -> Int -> [Int] -> Int
solve days remains as = remains + sum (map go as)
  where
    go a = length $ takeWhile (<=days) $ iterate (+a) 1