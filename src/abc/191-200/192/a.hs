main :: IO ()
main = do
  x <- readLn
  print $ solve x

solve :: Int -> Int
solve x =  100 - x `mod` 100