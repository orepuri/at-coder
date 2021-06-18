main :: IO ()
main = do
  [x, n] <- map read . words <$> getLine
  ps <- map read . words <$> getLine
  print $ solve x ps

solve :: Int -> [Int] -> Int
solve x ps = if abs (x - a1) < abs (x - a2)  then a1 else a2
  where
    a1 = head $ filter (`notElem` ps) [x,x+1..]
    a2 = head $ filter (`notElem` ps) [x-1,x-2..]
