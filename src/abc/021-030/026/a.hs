main :: IO ()
main = do
  a <- readLn
  print $ solve a

solve :: Int -> Int
solve a = maximum $ map (\n -> n * (a - n)) [1..a-1]