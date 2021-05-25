import Control.Monad
import Data.List
import qualified Data.IntSet as S

main :: IO ()
main = do
  as <- replicateM 3 $ map read . words <$> getLine
  n <- readLn
  bs <- replicateM n readLn
  putStrLn $ if solve as bs then "Yes" else "No"

solve :: [[Int]] -> [Int] -> Bool
solve as bs = any (all (`S.member` s)) lines
  where
    s = foldl' (flip S.insert) S.empty bs
    lines = as
      ++ transpose as
      ++ [[as !! 0 !! 0, as !! 1 !! 1, as !! 2 !! 2],[as !! 0 !! 2, as !! 1 !! 1, as !! 2 !! 0]]
