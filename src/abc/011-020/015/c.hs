import Data.Bits
import Control.Monad

main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine
  ts <- replicateM n $ do
    map read . words <$> getLine
  putStrLn $ if solve ts then "Found" else "Nothing"

solve :: [[Int]] -> Bool
solve ts = elem 0 $ map (foldr1 xor) $ sequence ts
