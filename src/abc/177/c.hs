import Data.List
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import Data.Char

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve as

solve :: VU.Vector Int -> Int
solve as = VU.foldl1' (+%) $ VU.tail $ VU.zipWith (*%) as $ VU.scanl' (+%) 0 as
  where
    a +% b = (a + b) `mod` m
    a *% b = (a * b) `mod` m

m :: Int
m = 10^9 + 7
