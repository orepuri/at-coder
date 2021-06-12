import Control.Monad

import qualified Data.ByteString.Char8 as C
import Data.Char

import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  n <- readLn :: IO Int
  hs <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  putStrLn $ if solve n hs then "Yes" else "No"

solve :: Int -> VU.Vector Int -> Bool
solve n hs = go (VU.head hs - 1) (VU.tail hs)
  where
    go ph hs
      | VU.null hs = True
      | ph > h = False
      | ph == h = go h hs'
      | otherwise = go (h - 1) hs'
      where
        h = VU.head hs
        hs' = VU.tail hs
