main :: IO ()
main = do
  n <- readLn
  print $ if even n then n `div` 2 else n `div` 2 + 1