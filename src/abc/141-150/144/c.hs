import Control.Monad

main :: IO ()
main = do
  n <- readLn :: IO Int
  print $ solve n

solve :: Int -> Int
solve n = minimum [x+(n`div`  x)-2| x <- [1..n'], n `mod` x == 0]
  where
    n' = ceiling $ sqrt $ fromIntegral n
