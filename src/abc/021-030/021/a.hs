import Control.Monad

main :: IO ()
main = do
  n <- readLn
  let ans = solve n
  print $ length ans
  forM_ ans print

solve :: Int -> [Int]
solve n
  | even n = let a = n `div` 2 in replicate a 2
  | otherwise = let a = n `div` 2 in 1 : replicate a 2

