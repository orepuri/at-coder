import Control.Monad
import Data.Char


main :: IO ()
main = do
  s <- getLine
  k <- readLn :: IO Int
  print $ solve s k

solve :: String -> Int -> Int
solve (a:as) k
  | null as = digitToInt a
  | all (== '1') $ take k (a:as) = 1
  | a == '1' = digitToInt $ head $ dropWhile (=='1') as
  | otherwise = digitToInt a


-- 1種類のみ
-- K番目まで全部1
-- 先頭が1で, K番目までに１以外がある
-- 先頭が1以外
