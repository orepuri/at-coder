main :: IO ()
main = do
  l <- readLn :: IO Int
  print $ solve l

solve :: Int -> Double
solve l = (fromIntegral l / 3) ^ 3
