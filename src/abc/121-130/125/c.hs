import Control.Monad

import qualified Data.ByteString.Char8 as C
import Data.Char

import qualified Data.Vector.Unboxed as VU

import Debug.Trace

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve n as

solve :: Int -> VU.Vector Int -> Int
solve n as = maximum $ map gcd' [0..n-1]
  where
    ls = VU.scanl' gcd 0 as
    rs = VU.scanr' gcd 0 as
    gcd' i = gcd (ls VU.! i) (rs VU.! (i+1))
