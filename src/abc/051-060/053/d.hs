import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import qualified Data.IntSet as S

import Data.Char

main :: IO ()
main = do
  n <- readLn
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve as

solve :: VU.Vector Int -> Int
solve as = if even (len - k) then k else k - 1
  where
    k = S.size $ VU.foldl' (flip S.insert) S.empty as
    len = VU.length as