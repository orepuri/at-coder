main :: IO ()
main = do
  n <- readLn
  print $ if n == 12 then 1 else n + 1