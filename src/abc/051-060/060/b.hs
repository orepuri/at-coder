main :: IO ()
main = do
  [a, b, c] <- map read . words <$> getLine
  putStrLn $ if solve a b c then "YES" else "NO"

solve :: Int -> Int -> Int -> Bool
solve a b c = any (\i -> a * i `mod` b == c) [1..b] 

-- xはAの倍数: x = nA
-- nA `mod` B は 0-(B-1) で周期する
-- その周期の中にCがあればOK