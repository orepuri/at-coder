main :: IO ()
main = do
  n <- readLn
  let x = 800 * n
  let y = n `div` 15 * 200
  print $ x - y