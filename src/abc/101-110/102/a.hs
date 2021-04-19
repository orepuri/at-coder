main :: IO ()
main = do
  n <- readLn
  print $ if even n then n else 2 * n