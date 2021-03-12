{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  n <- readLn
  s <- VU.unfoldrN n C.uncons <$> C.getLine
  print $ VU.maximum
    $ VU.map (\ !gap -> min gap $ test s $ VU.drop gap s)
    $ VU.tail $ VU.generate n id

test :: VU.Vector Char -> VU.Vector Char -> Int
test v1 v2 =
  VU.maximum
  $ VU.scanl' (\ !l !b -> if b then l+1 else 0) (0::Int)
  $ VU.zipWith (==) v1 v2