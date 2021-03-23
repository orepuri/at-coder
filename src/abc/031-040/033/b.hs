import Control.Monad
import Data.List

main :: IO ()
main = do
  n <- readLn
  sps <- replicateM n $ do
    [s, p] <- words <$> getLine
    return (s, read p)
  putStrLn $ solve sps

solve :: [(String, Int)] -> String
solve sps = case find (\(s, p) -> fromIntegral p > fromIntegral population / 2) sps of
    Just (s, p) -> s
    Nothing -> "atcoder"
  where
    population = sum $ map snd sps 