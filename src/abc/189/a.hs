main :: IO ()
main = do
  [c1, c2, c3] <- getLine
  putStrLn $ if c1 == c2 && c2 == c3 then "Won" else "Lost"
