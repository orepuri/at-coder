import Control.Monad

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- map read . words <$> getLine
  print $ solve as

solve :: [Int] -> Int
solve as = sum $ map f as
  where
    f a
     | even a = f (a - 1) + 1
     | (a + 1) `mod` 3 == 0 = f (a - 1) + 1
     | otherwise = 0
