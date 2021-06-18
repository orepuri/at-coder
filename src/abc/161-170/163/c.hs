import Control.Monad
import qualified Data.Array.Unboxed as AU
import qualified Data.ByteString.Char8 as C
import Data.Char

import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  let ans = solve n as
  forM_ [1..n] $ \i -> do
    print $ ans AU.! i

solve :: Int -> VU.Vector Int-> AU.UArray Int Int
solve n as = AU.accumArray (+) 0 (1, n) $
  zip (VU.toList as) (repeat 1)
