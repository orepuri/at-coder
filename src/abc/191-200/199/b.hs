main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- map read . words <$> getLine
  bs <- map read . words <$> getLine
  print $ solve as bs

solve :: [Int] -> [Int] -> Int
solve as bs = let (x, y) = loop as bs (1, 1000) in if x > y then 0 else y - x + 1
  where
    loop :: [Int] -> [Int] -> (Int, Int) -> (Int, Int)
    loop [] [] (x, y) = (x, y)
    loop (a:as) (b:bs) (x, y) = loop as bs (max a x, min b y)