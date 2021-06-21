import Control.Monad

import qualified Data.ByteString.Char8 as C
import Data.Char

import qualified Data.Vector.Unboxed as VU


main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve n as

solve :: Int -> VU.Vector Int -> Int
solve n as = if ans == n then -1 else ans
  where
    ans = go 1 0 as
    go i breaks as
      | VU.null as = breaks
      | a == i = go (i+1) breaks as'
      | otherwise = go i (breaks + 1) as'
      where
        a = VU.head as
        as' = VU.tail as
