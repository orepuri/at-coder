import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn
  sts <- replicateM n $ do
    [s, t] <- words <$> getLine
    return (s, read t)
  putStrLn $ solve n sts

solve :: Int -> [(String, Int)] -> String
solve n st = fst $ sorted !! 1
  where
    sorted = sortBy (\(_, h1) (_, h2) -> compare h2 h1) st