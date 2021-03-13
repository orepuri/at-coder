import Control.Monad

main :: IO ()
main = do
  [n, s, t] <- map read . words <$> getLine
  w <- readLn
  as <- replicateM (n - 1) readLn
  print $ solve s t w as

solve :: Int -> Int -> Int -> [Int] -> Int
solve s t w as = length $ filter (\w -> s <= w && w <= t) $ scanl (+) w as