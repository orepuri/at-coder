import Control.Monad

main :: IO ()
main = do
  n <- readLn :: IO Int
  bs <- map read . words <$> getLine
  print $ solve n bs

solve :: Int -> [Int] -> Int
solve n bs
  | n == 2 = head bs * 2
  | otherwise = sum middle + max (head bs) (head middle) + max (last bs) (last middle)
  where
    middle = zipWith min bs (tail bs)