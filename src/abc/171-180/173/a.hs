  main :: IO ()
  main = do
    n <- readLn
    print $ solve n

  solve :: Int -> Int
  solve n = if n <= 1000 then 1000 - n else solve $ n - 1000