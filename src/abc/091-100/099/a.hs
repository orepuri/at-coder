main :: IO ()
main = do
  n <- readLn :: IO Int
  putStrLn $ if n >= 1000 then "ABD" else "ABC"