main :: IO ()
main = do
  n <- readLn
  putStrLn $ solve n

solve :: Int -> String
solve n
  | n <= 59 = "Bad"
  | n <= 89 = "Good"
  | n <= 99 = "Great"
  | otherwise = "Perfect"