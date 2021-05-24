import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import Data.Char

main :: IO ()
main = do
  n <- readLn
  ds <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve n ds

solve :: Int -> VU.Vector Int -> Int
solve n ds = sum $ map (\(i, j) -> ds VU.! i * ds VU.! j) pairs 
  where
    pairs = [(i,j)| i <- [0..n-2], j <- [1..n-1], i < j]