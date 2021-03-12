{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Array as A
import qualified Data.Array.IO as IO
import qualified Data.Array.Unboxed as UA
import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxing as VUG
import qualified Data.Vector.Unboxing.Mutable as VUGM

main :: IO ()
main = do
  [h,w] <- readIntList
  grid <- readCharMatrixAsVec h w
  putStrLn =<< show <$> solve h w grid

solve :: Int -> Int -> VU.Vector Char -> IO ModInt
solve h w grid = do
  dp <- VUGM.replicate (h*w) (ModInt 0)
  dpR <- VUGM.replicate (h*w) (ModInt 0)
  dpC <- VUGM.replicate (h*w) (ModInt 0)
  dpRC <- VUGM.replicate (h*w) (ModInt 0)
  VUGM.write dpR 0 1
  VUGM.write dpC 0 1
  VUGM.write dpRC 0 1
  forM_ [0..h-1] $ \h' -> do
    forM_ [0..w-1] $ \w' -> do
      let idx = h' * w + w'
      if (h' == 0 && w' == 0) || grid VU.! idx == '#' then do
        pure ()
      else do
        r <- if (w' == 0) then pure 0
          else VUGM.read dpR (idx-1)
        c <- if (h' == 0) then pure 0
          else VUGM.read dpC (idx-w)
        rc <- if (w' == 0 || h' == 0) then pure 0
          else VUGM.read dpRC (idx-w-1)
        v <- VUGM.read dp idx
        let v' = v + r + c + rc
        VUGM.write dp idx v'
        VUGM.write dpR idx (v'+r)
        VUGM.write dpC idx (v'+c)
        VUGM.write dpRC idx (v'+rc)
  VUGM.read dp $ h*w-1

modulus = 10 ^ 9 + 7

newtype ModInt = ModInt Int deriving (Eq, Ord)
 
instance VUG.Unboxable ModInt where
    type Rep ModInt = Int
 
instance Num ModInt where
  (ModInt a) + (ModInt b) = ModInt $ mod (a + b) modulus
  (ModInt a) - (ModInt b) = ModInt $ mod (a - b) modulus
  (ModInt a) * (ModInt b) = ModInt $ mod (a * b) modulus
  fromInteger a = ModInt $ mod (fromInteger a) modulus
  abs (ModInt a) = undefined
  signum (ModInt a) = undefined
 
int2ModInt :: Int -> ModInt
int2ModInt a = ModInt (mod a modulus)
 
modInt2Int :: ModInt -> Int
modInt2Int (ModInt a) = a

instance Show ModInt where
  show (ModInt a) = show (mod a modulus)

readCharMatrixAsVec :: Int -> Int -> IO (VU.Vector Char)
readCharMatrixAsVec r c = VU.unfoldrN (r*c) parseChar . BS.concat <$> replicateM r BS.getLine

type Parser a = BS.ByteString -> Maybe (a, BS.ByteString)

parseChar :: Parser Char
parseChar = C.uncons

readInt :: IO Int
readInt = toInt <$> C.getLine

readIntList :: IO [Int]
readIntList = map toInt . C.words <$> C.getLine

readIntLists :: Int -> IO [[Int]]
readIntLists n = replicateM n readIntList


toInt :: BS.ByteString -> Int
toInt = fst . fromJust . C.readInt
