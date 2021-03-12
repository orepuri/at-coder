import Data.List

main :: IO ()
main = do
  n <- readLn
  mapM_ print $ sort $ factors 1 n

factors :: Int -> Int -> [Int]
factors _ 1 = [1]
factors i n
  | i * i > n = []
  | n `mod` i == 0 = if i * i == n then i : factors (i + 1) n else i : n `div` i : factors (i + 1) n
  | otherwise = factors (i + 1) n