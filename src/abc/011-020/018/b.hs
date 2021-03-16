import Control.Monad
import Data.List

main :: IO ()
main = do
  s <- getLine
  n <- readLn
  lrs <- replicateM n $ do
    [l, r] <- map read . words <$> getLine
    return (l, r)
  putStrLn $ solve s lrs

solve :: String -> [(Int, Int)] -> String
solve = foldl' f
  where
    f acc (l, r) = take (l-1) acc ++
     reverse (take (r-l+1) (drop (l-1) acc)) ++
     drop r acc
