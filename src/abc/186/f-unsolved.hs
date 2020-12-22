import Control.Monad
import Control.Monad.Primitive
import qualified Data.Array.IO as AIO
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

main :: IO ()
main = do
  [h,w,m] <- readIntList
  rows <- VUM.replicate (w+1) h
  cols <- VUM.replicate (h+1) w
  rocks <- AIO.newArray (1,h) [] :: IO (AIO.IOUArray Int [Int])
  forM_ [1..m] $ \_ -> do
    [r, c] <- readIntList
    VUM.modify rows (min c) r
    VUM.modify cols (min r) c
    AIO.writeArray rocks r [c]
  rows' <- VU.freeze rows
  cols' <- VU.freeze cols
  fenwick <- newFenwick (w+1)
  print $ "rows = " ++ show rows'
  print $ "cols = " ++ show cols'
  forM_ [1..cols' VU.! 0] $ \c -> do
    add fenwick c 1
  let rightDowns = VU.sum rows'
  let downRights = VU.sum cols'
  print $ rightDowns + downRights
  f <- VU.freeze fenwick
  print f

type Fenwick s = VUM.MVector s Int

newFenwick :: PrimMonad m => Int -> m (Fenwick (PrimState m))
newFenwick n = VUM.replicate n 0

add :: PrimMonad m => Fenwick (PrimState m) -> Int -> Int -> m ()
add vec i d | i <= 0 || i >= VUM.length vec = return ()
            | otherwise = do
              VUM.modify vec (+d) i
              add vec (i + (i .&. negate i)) d

sumFenwick :: PrimMonad m => Fenwick (PrimState m) -> Int -> m Int
sumFenwick vec i | i >= VUM.length vec = error "Illegal argument"
          | otherwise = go 0 i
  where
    go acc i | i <= 0 = return acc
             | otherwise = do
                 vi <- VUM.read vec i
                 go (acc + vi) (i .&. complement (i .&. negate i))

readIntList :: IO [Int]
readIntList = map toInt . C.words <$> BS.getLine

toInt :: BS.ByteString -> Int
toInt = fst . fromJust . C.readInt
