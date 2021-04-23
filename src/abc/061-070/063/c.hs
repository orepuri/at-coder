import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn
  ss <- replicateM n readLn :: IO [Int]
  print $ solve ss

solve :: [Int] -> Int
solve ss = if maxScore `mod` 10 /= 0
          then maxScore
          else case min of
            Just m -> maxScore - m
            Nothing -> 0
  where
    maxScore = sum ss
    min = find (\s -> s `mod` 10 /= 0) $ sort ss