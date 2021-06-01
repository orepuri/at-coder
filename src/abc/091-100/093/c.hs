import Control.Monad
import Data.List

main :: IO ()
main = do
  xs <- map read . words <$> getLine
  print $ solve $ sort xs

solve :: [Int]-> Int
solve [a, b, c] = if even d3
    then d1 + d3 `div` 2
    else d1 + d3 `div` 2 + 2
  where
    d1 = c - b
    d2 = a + d1
    d3 = c - d2
