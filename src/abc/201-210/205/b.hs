import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- map read . words <$> getLine
  putStrLn $ if isSatisfied n as then "Yes" else "No"

isSatisfied :: Int -> [Int] -> Bool
isSatisfied n as = sort as == [1..n]