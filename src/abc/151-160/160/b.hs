main :: IO ()
main = do
  x <- readLn
  print $ solve x

solve :: Int -> Int
solve x = i * 1000 + j * 5 
  where
    i = x `div` 500
    j = (x - i * 500) `div` 5