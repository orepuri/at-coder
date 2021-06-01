import Control.Monad

main :: IO ()
main = do
  [a, b, c, x, y] <- map read . words <$> getLine
  print $ solve a b c x y

solve :: Int -> Int -> Int -> Int -> Int -> Int
solve a b c x y = minimum [p1, p2, p3, p4]
  where
    p1 = a * x + b * y
    p2 = 2 * c * max x y
    p3 = if x >= y then 2 * c * y + a * (x - y) else 2 * c * y
    p4 = if y >= x then 2 * c * x + b * (y - x) else 2 * c * x

-- Aのみ, Bのみを買う
-- ABのみを買う
-- AB+Aを買う
-- AB+Bを買う