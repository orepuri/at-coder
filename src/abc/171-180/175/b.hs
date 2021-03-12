main :: IO ()
main = do
  n <- readLn :: IO Int
  ls <- map read . words <$> getLine
  print $ solve ls

solve :: [Int] -> Int
solve ls = length [1 | a <- ls, b <- ls, a < b, c <- ls, b < c, a + b > c]
