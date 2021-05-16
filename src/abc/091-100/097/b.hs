main :: IO ()
main = do
  x <- readLn
  print $ solve x

solve :: Int -> Int
solve x = maximum $ filter (<=x) [b^p| b <- [1..32], p <- [2..10]]
