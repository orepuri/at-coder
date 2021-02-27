import Data.List

main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine
  print $ solve n k

solve :: Int -> Int -> Int
solve n k = if k > 0 then let x' = solve n (k - 1) in g1 x' - g2 x' else n
  where
    g1 x = read $ sortBy (flip compare) $ show x
    g2 x = read $ sort $ show x

-- a0 = n
-- a1 = f a0
-- a2 = f a1 = f f a0


