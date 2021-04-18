import Data.List

main :: IO ()
main = do
  n <- readLn
  print $ solve n

solve :: Int -> Int
solve n = length $ show $ foldl' (\acc a -> if n `mod` a == 0 then min acc (n `div` a) else acc) n [1..maxA]
  where
    maxA = floor $ sqrt $ fromIntegral n
