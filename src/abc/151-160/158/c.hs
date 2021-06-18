main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  print $ solve a b

solve :: Int -> Int -> Int
solve a b = if null xs then -1 else head xs
  where
    xs = filter (\x -> floor (fromIntegral x * 1.08) == (x + a) && floor (fromIntegral x * 1.1) == (x + b)) [1..1250]
