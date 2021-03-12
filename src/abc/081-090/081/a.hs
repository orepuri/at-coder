main :: IO ()
main = do
  s <- getLine
  print $ sum [1 | c <- s, c == '1']