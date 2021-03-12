main :: IO ()
main = do
  x <- readLn
  print $ if x == 0 then 1 else 0