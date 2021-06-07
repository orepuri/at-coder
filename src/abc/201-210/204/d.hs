import Control.Monad

import qualified Data.Array.IO as AIO
import qualified Data.Array.Unboxed as AU
import qualified Data.ByteString.Char8 as C

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

import Data.Char

main :: IO ()
main = do
  n <- readLn :: IO Int
  ts <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  let total = VU.sum ts
      smin = total `div` 2
  dp <- AIO.newArray (0, total) False :: IO (AIO.IOArray Int Bool)
  AIO.writeArray dp 0 True
  VU.forM_ ts $ \t -> do
    forM_ [total - t, total - t - 1..0] $ \k -> do
      cur <- AIO.readArray dp k
      when cur $ AIO.writeArray dp (k + t) True
  dp' <- AIO.freeze dp :: IO (AU.UArray Int Bool)
  let go i = if dp' AU.! i then max i (total - i) else go (i-1)
  print $ go smin
