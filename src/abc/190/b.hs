import Control.Monad
import Data.List

main :: IO ()
main = do
  [n, s, d] <- map read . words <$> getLine
  xys <- replicateM n $ do
    [x, y] <- map read . words <$> getLine
    return (x, y)
  putStrLn $ if solve s d xys then "Yes" else "No"

solve :: Int -> Int -> [(Int, Int)] -> Bool
solve s d = any (\(x, y) -> x < s && y > d)