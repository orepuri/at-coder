import Control.Monad

main :: IO ()
main = do
  [a, b, c, d] <- map read . words <$> getLine
  print $ solve a b c d

solve :: Int -> Int -> Int -> Int -> Int
solve a b c d = b - a + 1 - divs c - divs d + divs (lcm c d)
  where
    divs n = b `div` n - (a - 1) `div` n
