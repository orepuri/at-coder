import Text.Printf

main :: IO ()
main = do
  n <- readLn :: IO Int
  printf "%.10f" $ solve n

solve :: Int -> Double
solve n = sum $ map (\i -> fromIntegral n / fromIntegral (n - i) ) [1..n-1]


-- 確率pの事象が起こるまでの期待値1/p

-- i種類訪問済みで新しい頂点にいける確率
-- (n - i)/n

-- i種類訪問済みで新しい頂点にいくまでの期待値
-- n / (n - i)