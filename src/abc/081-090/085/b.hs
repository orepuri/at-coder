import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn
  ds <- replicateM n readLn
  print $ solve n ds

solve :: Int -> [Int] -> Int
solve n = length . nub . sort
