import Control.Monad
import Data.List


main :: IO ()
main = do
  n <- readLn :: IO Int
  vs <- map read . words <$> getLine
  print $ solve n vs

solve :: Int -> [Double] -> Double
solve n vs = foldl1' (\acc v -> (acc + v) / 2) $ sort vs
