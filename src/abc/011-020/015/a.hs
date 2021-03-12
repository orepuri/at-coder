main :: IO ()
main = do
  a <- getLine
  b <- getLine
  putStrLn $ if length a > length b then a else b