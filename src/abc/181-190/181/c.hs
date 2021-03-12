import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn :: IO Int
  xys <- replicateM n $ do
    [x, y] <- map read . words <$> getLine
    return (x, y)
  putStrLn $ if solve xys then "Yes" else "No"

solve :: [(Int, Int)] -> Bool
solve xys = any isSameSlope comb
  where
    comb = [[p1, p2, p3] | p1 <- xys, p2 <- xys, p1 /= p2, p3 <- xys, p2 /= p3, p3 /= p1]

isSameSlope :: [(Int, Int)] -> Bool
isSameSlope [(x1, y1), (x2, y2), (x3, y3)] = x2' * y3' == x3' * y2'
  where
    x2' = x2 - x1
    y2' = y2 - y1
    x3' = x3 - x1
    y3' = y3 - y1
    