import Control.Monad

import qualified Data.ByteString.Char8 as C
import Data.Char

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VAI
import Debug.Trace

main :: IO ()
main = do
  n <- readLn :: IO Int
  ls <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  ls' <- VU.thaw ls
  VAI.sort ls'
  print . solve n =<< VU.freeze ls'

solve :: Int -> VU.Vector Int-> Int
solve n ls = sum $ map go1 [0..n-3]
  where
    go1 p1 = sum $ map (go2 p1) [p1+1,p1+2..n-2]
    go2 p1 p2 = max 0 (idx - p2 - 1)
      where
        idx = firstIndexGreaterThanOrEqual n ls (ls VU.! p1 + ls VU.! p2)

firstIndexGreaterThanOrEqual :: Int -> VU.Vector Int -> Int -> Int
firstIndexGreaterThanOrEqual n xs v
  | VU.last xs < v = n
  | otherwise =  go 0 (n - 1)
  where
    go ng ok
      | ng >= ok = ok
      | isOK (xs VU.! mid) = go ng mid
      | otherwise = go (mid + 1) ok
      where
        mid = (ng + ok) `div` 2
        isOK x = x >= v
