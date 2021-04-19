main :: IO ()
main = do
  r <- readLn
  putStrLn $ if r < 1200 then "ABC" else if r < 2800 then "ARC" else "AGC"