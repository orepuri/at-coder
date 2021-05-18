import Data.List
import Data.Maybe

main :: IO ()
main = do
  n <- readLn :: IO Int
  [t, a] <- map read . words <$> getLine
  hs <- map read . words <$> getLine
  print $ solve t a hs

solve :: Int -> Int -> [Int] -> Int
solve t a hs = 1 + fromJust (elemIndex (minimumBy (\h1 h2 -> compare (temp h1) (temp h2)) hs) hs)
  where
    temp h = abs $ 1000 * a - (1000 * t - h * 6)