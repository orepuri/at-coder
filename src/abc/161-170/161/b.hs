main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  as <- map read . words <$> getLine
  putStrLn $ if solve m as then "Yes" else "No"

solve :: Int -> [Int] -> Bool
solve m as = m <= length (filter (\a -> fromIntegral a >= (t / (4 * fromIntegral m))) as)
  where
    t = fromIntegral $  sum as