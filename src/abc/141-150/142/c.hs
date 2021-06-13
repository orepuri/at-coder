import Control.Monad
import Control.Monad.Primitive

import qualified Data.ByteString.Char8 as C
import Data.Char

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VAI

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  let asi = VU.zip as $ VU.iterateN n (+1) 1 :: VU.Vector (Int, Int)
  mut <- VU.thaw asi
  VAI.sortBy (\(a1, i1) (a2, i2) -> compare a1 a2) mut
  sorted <- VU.freeze mut
  putStrLn $ unwords $ map (\(_, i) -> show i ) $ VU.toList sorted
