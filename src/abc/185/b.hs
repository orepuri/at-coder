import Control.Monad

main :: IO ()
main = do
  [n, m, t] <- map read . words <$> getLine
  abs <- replicateM m $ do
    [a, b] <- map read . words <$> getLine
    pure (a, b)
  putStrLn $ if solve n n 0 t abs then "Yes" else "No"

solve :: Int -> Int -> Int -> Int -> [(Int, Int)] -> Bool
solve remain _ prevTime t [] = remain - (t - prevTime) > 0
solve remain v prevTime t ((a, b):tail)
  | remainA <= 0 = False
  | remainB <= 0 = False
  | otherwise    = solve remainB v b t tail
  where
    remainA = remain - (a - prevTime)
    remainB = min v $ remainA + (b - a)