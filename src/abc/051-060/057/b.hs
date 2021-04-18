import Control.Monad
import Data.List
import Data.Maybe

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  abs <- replicateM n $ do
    [a, b] <- map read . words <$> getLine
    return (a, b)
  cds <- replicateM m $ do
    [c, d] <- map read . words <$> getLine
    return (c, d)
  forM_ abs $ \(a, b) -> do
    print $ solve a b cds

solve :: Int -> Int -> [(Int, Int)] -> Int
solve a b cds = m
  where
    m = fst $ minimumBy (\i j -> compare (snd i) (snd j)) $ map (\(i, (c, d)) -> (i, abs (a - c) + abs (b - d))) $ zip [1..] cds