main :: IO ()
main = do
  a <- readLn
  b <- readLn
  n <- readLn
  print $ head [x | x <- [n..], x `mod` a == 0, x `mod` b == 0]