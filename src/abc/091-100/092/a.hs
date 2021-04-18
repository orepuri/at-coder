main :: IO ()
main = do
  a <- readLn
  b <- readLn
  c <- readLn
  d <- readLn
  print $ min a b + min c d