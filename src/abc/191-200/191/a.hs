main :: IO ()
main = do
  [v, t, s, d] <- map read . words <$> getLine
  putStrLn $ if solve v t s d then "Yes" else "No"

solve :: Int -> Int -> Int -> Int -> Bool
solve v t s d = not $ v * t <= d && d <= v * s

