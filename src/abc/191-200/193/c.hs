import qualified Data.Set as S

main :: IO ()
main = do
  n <- readLn
  print $ solve n

solve :: Int -> Int
solve n = n - ans
  where
    ans = S.size $ S.fromList [c | a <- [2..amax], b <- [2..bmax a], let c = a ^ b, c <= n]
    amax = floor $ sqrt $ fromIntegral n
    bmax a = floor $ logBase (fromIntegral a) (fromIntegral n)
