main :: IO ()
main = do
  x <- readLn
  putStrLn $ if x >= 30 then "Yes" else "No"