import Control.Monad
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
  input <- replicateM 6 readLn
  print $ solve input

solve :: [Int] -> Int
solve [n, a, b, c, d, e]
  | n <= minPath = 5
  | otherwise = 4 + ceiling (fromIntegral n / fromIntegral minPath)
  where
    minPath = minimum [a, b, c, d, e]

