import Control.Monad

main :: IO ()
main = do
  n <- readLn
  ps <- replicateM n readLn
  print $ solve ps

solve :: [Int] -> Int
solve ps = (high `div` 2) + sum ps - high
  where
    high = maximum ps
