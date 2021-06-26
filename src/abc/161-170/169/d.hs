{-# LANGUAGE BangPatterns #-}


import Data.List


main :: IO ()
main = do
  n <- readLn
  print $ solve n

solve :: Int -> Int
solve n = sum $ map go $ group $ primeFactors n 
  where
    go ps = length $ takeWhile (<= length ps)  $ tail $ scanl' (+) 0 [1..]

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = loop1 n
  where
    loop1 n
      | even n = 2 : loop1 (n `div` 2)
      | otherwise = loop2 n 3
    loop2 n d
      | n < d * d = [ n | n /= 1 ]
      | otherwise = case n `divMod` d of
        (p, 0) -> d : loop2 p d
        _ -> loop2 n (d + 2)