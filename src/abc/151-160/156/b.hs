import Data.List
import Data.Maybe

main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine
  print $ fromJust $ findIndex (>n) $ map (k^) [0..]
