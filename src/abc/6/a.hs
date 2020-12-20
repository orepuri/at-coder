main :: IO ()
main = do
  n <- readLn :: IO Int
  putStrLn $ if n `mod` 3 == 0 then "YES" else "NO"