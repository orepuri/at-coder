main :: IO ()
main = do
  n <- readLn
  print $ solve n

solve :: Int -> Int
solve n = s' * s'
  where
    s' = floor $ sqrt $ fromIntegral n