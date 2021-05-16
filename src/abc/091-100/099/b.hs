main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  print $ (b - a) * (b - a + 1) `div` 2 - b

solve :: Int -> Int -> Int
solve a b = t1 - a
  where
    towers = map (\i -> sum [1..i]) [1..999]
    pairs = zip towers $ tail towers
    (t1, t2) = head $ filter (\(t1, t2) -> (t1 - a) == (t2 - b)) pairs


-- Xまでの塔とX-1までの塔の高さの差はX
-- 埋もれてない高さがbの塔の高さは (1+2+...b-a) // X = b - a 
-- (1+2+..b-a) = (b-a) * (b-a+1) / 2
-- よって埋もれている雪の高さは (b-a) * (b-a+1) / 2 - b