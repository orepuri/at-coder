import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import Data.Char

main :: IO ()
main = do
  [n, a, b] <- map read . words <$> getLine
  xs <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve a b xs

solve :: Int -> Int -> VU.Vector Int -> Int
solve a b xs = snd $ VU.foldl' go (VU.head xs, 0) $ VU.tail xs
  where
    go (px, acc) x = (x, min (acc + b) (acc + ea))
      where
        ea = a * (x - px)
