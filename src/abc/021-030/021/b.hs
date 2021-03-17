import qualified Data.Set as S

main :: IO ()
main = do
  n <- readLn :: IO Int
  [a, b] <- map read . words <$> getLine
  k <- readLn
  ps <- map read . words <$> getLine
  putStrLn $ if solve a b k ps then "YES" else "NO"

solve :: Int -> Int -> Int -> [Int] -> Bool
solve a b k ps = length x == S.size y
  where
    x = a : b : ps
    y = S.fromList x