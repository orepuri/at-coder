main :: IO ()
main = do
  a <- readLn
  b <- readLn
  let (x, y) = if a <= b then (abs (a - b), a + (10 - b)) else (abs (b - a), b + (10 - a))
  print $ min x y
