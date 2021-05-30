import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.Char

up :: Integer
up = 10 ^ 18

main :: IO ()
main = do
  n <- readLn
  as <- V.unfoldrN n (C.readInteger . C.dropWhile isSpace) <$> C.getLine
  print $ solve as

solve :: V.Vector Integer -> Integer
solve as
  | zero = 0
  | otherwise = go 1 (V.head as) (V.tail as) 
  where
    zero = V.any (==0) as
    go acc a as
      | acc * a > up = -1
      | V.null as = if acc * a > up then -1 else acc * a
      | otherwise = go (acc * a) (V.head as) (V.tail as)
