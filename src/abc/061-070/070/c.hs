import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn
  ts <- replicateM n readLn :: IO [Int]
  print $ solve ts

solve :: [Int] -> Int
solve = foldl' lcm 1
