import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.Char

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VAI

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  as' <- VU.thaw as
  VAI.sort as'
  x <- VUM.read as' (n `div` 2 - 1)
  y <- VUM.read as' (n `div` 2)
  VU.forM_ (solve n as x y) print

solve :: Int -> VU.Vector Int -> Int -> Int -> VU.Vector Int
solve n as x y = VU.map (\a -> if x == y then x else if a < y then y else x) as
