main :: IO ()
main = do
  a <- readLn :: IO Integer
  b <- readLn
  putStrLn $ if a == b then "EQUAL" else if a > b then "GREATER" else "LESS"