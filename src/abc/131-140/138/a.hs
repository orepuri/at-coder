main :: IO ()
main = do
  a <- readLn
  s <- getLine
  putStrLn $ if a >= 3200 then s else "red"