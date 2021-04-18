main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  putStrLn $ if solve a b then "Yay!" else ":("

solve :: Int -> Int -> Bool
solve a b = a <= 8 && b <= 8


-- 1 15 -> x
-- 2 14 -> x
-- 3 13 -> x
-- 4 12 -> x
-- 5 11 -> x
-- 6 10 -> x
-- 7 