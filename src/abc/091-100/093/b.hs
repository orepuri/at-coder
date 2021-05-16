import Control.Monad
import Data.List

main :: IO ()
main = do
  [a, b, k] <- map read . words <$> getLine
  let ans = solve a b k
  forM_ ans print

solve :: Int -> Int -> Int -> [Int]
solve a b k = nub $ [a..min b (a+k-1)] ++ [max a (b-k+1)..b]
