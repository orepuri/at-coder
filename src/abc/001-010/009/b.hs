import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn
  as <- replicateM n readLn :: IO [Int]
  print $ head . tail . reverse . nub $ sort as 