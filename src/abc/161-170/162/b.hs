main :: IO ()
main = do
  n <- readLn
  print $ solve n

solve :: Int -> Int
solve n = sum $ take n $ map (\i -> if i `mod` 3 /= 0 && i `mod` 5 /= 0 then i else 0) [1..]
