main :: IO ()
main = do
  s <- getLine
  i <- readLn
  putChar $ s !! (i - 1)