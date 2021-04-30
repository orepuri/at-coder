main :: IO ()
main = do
  [c1,c2,c3,c4,c5,c6] <- getLine
  putStrLn $ if c3 == c4 && c5 == c6 then "Yes" else "No"