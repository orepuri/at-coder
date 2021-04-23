import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  abs <- replicateM m $ do
    [a, b] <- map read . words <$> getLine
    return (a, b)
  let loads = solve abs
  forM_ [1..n] $ \i -> do
    print $ fromMaybe 0 $ M.lookup i loads

solve :: [(Int, Int)] -> M.Map Int Int
solve = foldl' (\acc (a, b) -> M.insertWith (+) b 1 $ M.insertWith (+) a 1 acc) M.empty
