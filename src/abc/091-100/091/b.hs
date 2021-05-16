import Control.Monad
import Data.List
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  n <- readLn
  ss <- replicateM n getLine
  m <- readLn
  ts <- replicateM m getLine
  print $ solve ss ts

solve :: [String] -> [String] -> Int
solve ss ts = if ans < 0 then 0 else ans
  where
    sc = foldl' (\acc s -> M.insertWith (+) s 1 acc) M.empty ss
    tc = foldl' (\acc s -> M.insertWith (flip (-)) s 1 acc) sc ts
    ans = snd 
      $ minimumBy (\(_, c1) (_, c2) -> compare c2 c1) 
      $ M.toAscList tc