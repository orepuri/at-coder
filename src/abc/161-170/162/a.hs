main :: IO ()
main = do
  n <- getLine
  putStrLn $ if '7' `elem` n then "Yes" else "No"
