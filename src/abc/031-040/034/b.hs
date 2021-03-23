main :: IO ()
main = do
  n <- readLn
  print $ if even n then n-1 else n+1