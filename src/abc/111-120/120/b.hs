main :: IO ()
main = do
  [a, b, k] <- map read . words <$> getLine
  print $ solve a b k

solve :: Int -> Int -> Int -> Int
solve a b k = xs !! (length xs - k)
  where
    xs = [i|i <- [1..min a b], a `mod` i == 0, b `mod` i == 0]
