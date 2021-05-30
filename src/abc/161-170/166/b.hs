import Control.Monad
import qualified Data.IntSet as S
import Data.List

main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine
  ass <- replicateM k $ do
    d <- readLn :: IO Int
    map read . words <$> getLine
  print $ solve n ass

solve :: Int -> [[Int]] -> Int
solve n ass = n - S.size ns 
  where
    ns = foldl' (flip S.insert) S.empty $ concat ass