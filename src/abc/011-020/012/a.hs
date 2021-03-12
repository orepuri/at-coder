import Control.Monad
import Data.List

main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine :: IO [Int]
  putStrLn $ unwords $ map show [b, a]