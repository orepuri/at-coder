main :: IO ()
main = do
  n <- readLn
  let cs = takeWhile (<=n) $ iterate (*2) 1
  print $ if null cs then 0 else last cs