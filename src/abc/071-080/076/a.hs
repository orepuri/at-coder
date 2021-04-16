main :: IO ()
main = do
  s1 <- getLine
  s2 <- getLine
  putStrLn $ if s1 == reverse s2 then "YES" else "NO"
