main :: IO ()
main = do
  x <- readLn
  a <- readLn
  b <- readLn
  let r = x - a
      donuts = r `div` b
  print $ r - donuts * b