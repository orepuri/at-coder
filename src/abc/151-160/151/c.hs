import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.List
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Data.Maybe
import qualified Data.Vector.Unboxed as VU


main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  pss <- VU.replicateM m $ do
    [p, s] <- words <$> getLine
    return (read p, s == "AC")
  let (ok, ng) = solve n pss
  putStrLn $ unwords $ map show [ok, ng]

solve :: Int -> VU.Vector (Int, Bool) -> (Int, Int)
solve n pss = (IS.size allPassed, penalty)
  where
    allPassed = VU.foldl' (\acc (p, s) -> if s then IS.insert p acc else acc) IS.empty pss
    (_, penalty) = VU.foldl' go (IS.empty, 0) pss
    go (passed, penalty) (p, s)
      | s = (IS.insert p passed, penalty)
      | otherwise = (passed, if IS.member p passed then penalty else if IS.member p allPassed then penalty + 1 else penalty)
