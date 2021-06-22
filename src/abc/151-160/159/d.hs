import Control.Monad

import qualified Data.ByteString.Char8 as C
import Data.Char

import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  VU.forM_ (solve n as) print

solve :: Int -> VU.Vector Int -> VU.Vector Int
solve n as = VU.map (\a -> total - (freqs VU.! a - 1)) as
  where
    total = VU.sum patterns
    freqs :: VU.Vector Int
    freqs = VU.accumulate (+) (VU.replicate (n+1) 0) $ VU.map (\a -> (a, 1)) as
    patterns = VU.map (\n -> n * (n-1) `div` 2) freqs
