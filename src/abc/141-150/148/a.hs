main :: IO ()
main = do
  a <- readLn
  b <- readLn
  print $ head $ filter (\x -> x /= a && x /= b) [1,2,3]