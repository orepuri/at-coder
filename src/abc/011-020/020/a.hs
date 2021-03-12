main :: IO ()
main = do
  q <- readLn
  putStrLn $ if q == 1 then "ABC" else "chokudai"