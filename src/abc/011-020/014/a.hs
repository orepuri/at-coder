main :: IO ()
main = do
  a <- readLn
  b <- readLn
  print $ if a `mod` b == 0 then 0 else let n = a `div` b in b * (n + 1) - a