main :: IO ()
main = do
  x <- readLn
  print $ solve x

solve :: Integer -> Int
solve x = length
  $ takeWhile (<x)
  $ iterate (\x -> x + x `div` 100) 100