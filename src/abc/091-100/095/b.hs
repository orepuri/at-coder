import Control.Monad

main :: IO ()
main = do
  [n, x] <- map read . words <$> getLine
  ms <- replicateM n readLn
  print $ solve x ms

solve :: Int -> [Int] -> Int
solve x ms = length ms + remains `div` minCost
  where
    req = sum ms
    remains = x - req
    minCost = minimum ms