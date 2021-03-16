main :: IO ()
main = do
  a <- readLn
  print $ a + a ^ 2 + a ^ 3