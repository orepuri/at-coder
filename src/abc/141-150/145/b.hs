main :: IO ()
main = do
  n <- readLn
  s <- getLine
  putStrLn $ if solve n s then "Yes" else "No"

solve :: Int -> String -> Bool
solve n s = let (t1, t2) = splitAt (n `div` 2) s in t1 == t2