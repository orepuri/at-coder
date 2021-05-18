import Control.Monad
import Data.List

main :: IO ()
main = do
  [gh, gw] <- map read . words <$> getLine
  grid <- replicateM gh getLine
  putStr $ solve grid 

solve :: [String] -> String
solve = unlines 
    . transpose
    . filter (elem '#')
    . transpose
    . filter (elem '#')

