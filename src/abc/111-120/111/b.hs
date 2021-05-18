main :: IO ()
main = do
  n <- readLn
  print $ head $ dropWhile (<n) [111,222..999]
  
