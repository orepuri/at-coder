main :: IO ()
main = do
  n <- readLn :: IO Int
  print $ if even n then 0.5 else fromIntegral (n `div` 2 + 1) / fromIntegral n