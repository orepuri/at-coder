import Data.Bits

main :: IO ()
main = do
  [n, x] <- map read . words <$> getLine
  an <- map read . words <$> getLine
  print $ solve n x an

solve :: Int -> Int -> [Int] -> Int
solve n x an = sum $ map (\i -> if testBit x i then an !! i else 0) [0..n-1]