main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine
  print $ solve n k

solve :: Int -> Int -> Double
solve n k = (1/n')^3 + 3*(1/n')^2*s + 3*(1/n')^2*l + s*l*(1/n')*6
  where
    n' = fromIntegral n
    k' = fromIntegral k
    s = (k'-1)/n'
    l = (n'-k')/n'

--- k k k
--- s k k
--- k k l
--- s k l
