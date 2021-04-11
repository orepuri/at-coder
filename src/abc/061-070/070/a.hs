main :: IO ()
main = do
  [n1,_,n3] <- getLine
  putStrLn $ if n1 == n3 then "Yes" else "No"
