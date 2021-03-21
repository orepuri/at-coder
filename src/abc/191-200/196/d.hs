import Control.Monad
import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.Maybe
import Data.Bits

main :: IO ()
main = do
  [h, w, a, b] <- map read . words <$> getLine
  print $ solve h w a b

solve :: Int -> Int -> Int -> Int -> Int
solve h w a b = dfs 0 0 a b
  where
    dfs :: Int -> Int -> Int -> Int -> Int
    dfs table idx a b
      | idx >= h * w = 1
      | testBit table idx = dfs table (idx+1) a b
      | otherwise = tate table idx a b + yoko table idx a b + one table idx a b
    one table idx a b = if b < 1
      then 0
      else dfs (fill table idx) (idx+1) a (b-1)
    yoko table idx a b 
      | a < 1 = 0
      | (idx+1) `mod` w == 0 = 0
      | otherwise = dfs (fillH table idx) (idx+1) (a-1) b
    tate table idx a b
      | a < 1 = 0
      | idx >= w*h-1 = 0
      | otherwise = dfs (fillV table idx) (idx+1) (a-1) b
    fill table idx = setBit table idx
    fillH table idx = setBit (setBit table idx) (idx+1)
    fillV table idx = setBit (setBit table idx) (idx+w)
