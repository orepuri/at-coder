main :: IO ()
main = do
  n <- getLine
  putStrLn $ if solve n then "Yes" else "No"

solve [c1,c2,c3,c4] = c1 == c2 && c2 == c3 || c2 == c3 && c3 == c4