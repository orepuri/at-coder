main :: IO ()
main = do
  a <- readLn
  b <- readLn
  c <- readLn
  d <- readLn
  e <- readLn
  k <- readLn
  putStrLn $ if solve [a, b, c, d, e] k then ":(" else "Yay!"

solve :: [Int] -> Int -> Bool
solve xs k = any (\(a, b) -> abs (a - b) > k) [(a, b) | a <- xs, b <- xs, a /= b]
