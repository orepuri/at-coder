import Data.List

main :: IO ()
main = do
  n <- readLn
  as <- map read . words <$> getLine
  print $ solve n as

solve :: Int -> [Int] -> Int
solve n as = a - b
  where
    (a, b) = foldl'
             (\(as, bs) (i, n) -> if odd i
               then (as + n, bs)
               else (as, bs + n)
             )
             (0, 0)
             $ zip [1..] $ sortBy (flip compare) as