main :: IO ()
main = do
  n <- readLn
  ls <- map read . words <$> getLine
  putStrLn $ if solve n ls then "Yes" else "No"

solve :: Int -> [Int] -> Bool
solve n ls = maxL < sum ls - maxL
  where
    maxL = maximum ls