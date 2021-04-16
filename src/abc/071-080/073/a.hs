main :: IO ()
main = do
  n <- getLine
  putStrLn $ if '9' `elem` n then "Yes" else "No"