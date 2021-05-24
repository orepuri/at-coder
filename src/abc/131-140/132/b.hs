import Data.List

main :: IO ()
main = do
  n <- readLn :: IO Int
  ps <- map read . words <$> getLine :: IO [Int]
  print $ length $ filter (\[a,b,c] -> sort [a,b,c] !! 1 == b) $ filter (\ps -> length ps == 3) $ map (take 3) $ tails ps