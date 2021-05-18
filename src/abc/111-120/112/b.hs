import Control.Monad
import Data.List
import Data.Function

main :: IO ()
main = do
  [n, t] <- map read . words <$> getLine
  routes <- replicateM n $ do
    [c, t] <- map read . words <$> getLine
    return (c, t)
  let ans = solve n t routes  
  if ans < 0 then putStrLn "TLE" else print ans

solve :: Int -> Int -> [(Int, Int)] -> Int
solve n t routes = if null cands then -1 else fst $ minimumBy (compare `on` fst) cands
  where
    cands = filter (\(_, t') -> t' <= t) routes
