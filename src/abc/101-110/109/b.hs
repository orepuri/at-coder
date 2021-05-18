import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn
  ws <- replicateM n getLine
  putStrLn $ if solve ws then "Yes" else "No"

solve :: [String] -> Bool
solve (w:ws) = uniq && snd (foldl' (\(c, acc) w -> if head w == c then (last w, acc) else (last w, False)) (last w, True) ws)
  where
    uniq = length (w:ws) == length (nub (w:ws))