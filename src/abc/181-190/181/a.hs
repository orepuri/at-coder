main :: IO ()
main = do
  n <- readLn :: IO Int
  putStrLn $ if even n then "White" else "Black"