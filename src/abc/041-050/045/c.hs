import Data.Char

main :: IO ()
main = do
  s <- map digitToInt <$> getLine :: IO [Int]
  print $ solve s

solve :: [Int] -> Int
solve (s:ss) = go 0 s ss
  where
    go :: Int -> Int -> [Int] -> Int
    go acc s [] = acc + s
    go acc s (r:rs) = go (acc + s) r rs + go acc (s * 10 + r) rs
    
-- 残りがなければ最後の数値を足す
-- 残りがある場合, 1つの数として足すパターンと連結するパターンの合計
