import Control.Monad
import qualified Data.IntSet as S
import Data.List

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  questions <- replicateM n $ do
    result <- map read . words <$> getLine :: IO [Int]
    return $ tail result
  print $ solve questions

solve :: [[Int]] -> Int
solve (q:questions) = S.size
  $ foldl' (\acc q -> S.foldl' (\acc a -> if a `elem` q then acc else S.delete a acc) acc acc) (S.fromList q) questions