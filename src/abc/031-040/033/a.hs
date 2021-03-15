main :: IO ()
main = do
  n <- getLine
  let h = head n
  putStrLn $ if all (==h) n then "SAME" else "DIFFERENT"

