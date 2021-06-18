main :: IO ()
main = do
  x <- readLn
  print $ solve x

solve :: Int -> Int
solve x = head $ dropWhile (not . isPrime) [x..]

isPrime :: Int -> Bool
isPrime x = all (\n -> x `mod` n /= 0) [2..m]
  where
    m = floor $ sqrt $ fromIntegral x
