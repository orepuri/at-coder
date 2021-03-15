main :: IO ()
main = do
  s <- getLine
  putStrLn $ if 'T' == last s then "YES" else "NO"