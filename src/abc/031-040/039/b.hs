main :: IO ()
main = do
  x <- readLn :: IO Double
  print $ floor $ sqrt $ sqrt x