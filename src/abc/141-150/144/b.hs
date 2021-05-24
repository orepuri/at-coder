main :: IO ()
main = do
  n <- readLn
  putStrLn $ if solve n then "Yes" else "No"

solve :: Int -> Bool
solve n = n `elem` [i*j|i <- [1..9], j <- [1..9]]