main :: IO ()
main = do
  [x, y, a, b] <- map read . words <$> getLine
  let kakos' = kakos x y a b 0
  let remain = y - x * (a ^ kakos') - 1
  print $ kakos' + remain `div` b

kakos :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
kakos x y a b e
  | x' >= y || x' > x + b = e
  | otherwise = kakos x' y a b (e + 1)
  where
    x' = x * a