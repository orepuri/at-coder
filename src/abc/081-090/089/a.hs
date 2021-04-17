main :: IO ()
main = do
  n <- readLn
  print $ if n < 3 then 0 else floor $ n / 3