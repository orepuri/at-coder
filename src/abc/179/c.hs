import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  n <- readLn
  print $ solve n

solve :: Int -> Int
solve n = VU.sum $ VU.map (\a -> (n - 1) `div` a) $ VU.enumFromTo 1 (n - 1)

-- a * b
-- -> aを決めるとa*bはaの倍数
-- -> n - 1 以下の正整数で Aの倍数はいくつあるか
-- -> (n-1)/a
-- -> a*1, a*2, a*3, ... a * x < n - 1
-- -> 1,2,3 ... x < (n-1)/a