import Control.Monad
import Data.List

main :: IO ()
main = do
  [n, x] <- map read . words <$> getLine
  vps <- replicateM n $ do
    [v, p] <- map read . words <$> getLine
    return (v, p)
  print $ solve n (x*100) vps

solve :: Int -> Int -> [(Int, Int)] -> Int
solve n x vps = if n' == n then -1 else n' + 1
  where
    n' = length $ takeWhile (<=x) $ tail $ scanl' (+) 0 $ map (\(v,p) -> v*p) vps
