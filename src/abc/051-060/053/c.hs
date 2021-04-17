main :: IO ()
main = do
  x <- readLn
  print $ solve x

solve :: Int -> Int
solve x
  | x <= 6 = 1 
  | x <= 11 = 2
  | x `mod` 11 == 0 = 2 * (x `div` 11)
  | x `mod` 11 <= 6 = 2 * (x `div` 11) + 1
  | otherwise = 2 * (x `div` 11) + 2
