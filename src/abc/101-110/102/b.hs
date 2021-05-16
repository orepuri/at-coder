main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- map read . words <$> getLine
  print $ solve as

solve :: [Int] -> Int
solve as = maximum as - minimum as