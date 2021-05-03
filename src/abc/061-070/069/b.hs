main :: IO ()
main = do
  s <- getLine
  putStrLn $ head s : show (length s - 2) ++ [last s]