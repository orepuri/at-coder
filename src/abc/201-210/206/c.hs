import Control.Monad

import qualified Data.ByteString.Char8 as C
import Data.Char

import qualified Data.IntMap as IM

import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve n as
  
solve :: Int -> VU.Vector Int -> Int
solve n as = total - ngs
  where
    total = n * (n - 1) `div` 2
    ngs = sum $ map (\(_, n) -> n * (n - 1) `div` 2) $ IM.toList freqs
    freqs = VU.foldl' (\acc a -> IM.insertWith (+) a 1 acc) IM.empty as
