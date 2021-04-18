main :: IO ()
main = do
  s <- getLine
  print $ 700 + 100 * length (filter (=='o') s)