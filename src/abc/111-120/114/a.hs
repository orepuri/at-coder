main :: IO ()
main = do
  x <- readLn
  putStrLn $ if x `elem` [7,5,3] then "YES" else "NO"