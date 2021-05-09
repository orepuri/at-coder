main :: IO ()
main = do
  n <- readLn
  print $ if n `mod` 100 == 0 then n `div` 100 else n `div` 100 + 1