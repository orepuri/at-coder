main :: IO ()
main = do
  s <- getLine
  putStrLn $ if s == "ABC" then "ARC" else "ABC"