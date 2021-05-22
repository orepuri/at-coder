import Control.Monad
import Data.List
import Data.Char


main :: IO ()
main = do
  times <- replicateM 5 readLn
  print $ solve times

solve :: [Int] -> Int
solve times = head sorted + sum (map roundUp $ tail sorted)
  where
    sorted = sortBy cmpFstDigit times

roundUp t = 10 * ceiling (fromIntegral t / 10)
fstDigit n = digitToInt $ last $ show n
cmpFstDigit t1 t2
    | t1' == 0 = GT
    | t2' == 0 = LT
    | otherwise = compare t1' t2'
  where
    t1' = fstDigit t1
    t2' = fstDigit t2