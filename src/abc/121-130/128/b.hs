import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn
  sps <- forM [1..n] $ \i -> do 
    [s, p] <- words <$> getLine
    return (i, s, read p)
  let ans = solve sps
  forM_ ans print

solve :: [(Int, String, Int)] -> [Int]
solve sps = map (\(i, _, _) -> i) 
  $ sortBy comp sps
  where
    comp (_, s1, p1) (_, s2, p2) = compare s1 s2 <> compare p2 p1
