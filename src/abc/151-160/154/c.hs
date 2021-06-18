import Control.Monad

import qualified Data.ByteString.Char8 as C
import Data.Char

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VAI

main :: IO ()
main = do
  n <- readLn
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  mas <- VU.thaw as
  VAI.sortBy (flip compare) mas
  as' <- VU.freeze mas
  putStrLn $ if solve n as' then "YES" else "NO"

solve :: Int -> VU.Vector Int -> Bool
solve n as = VU.foldl' (\acc (a, na) -> acc && a /= na) True $ VU.zip as (VU.tail as)
