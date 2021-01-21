{-# LANGUAGE TypeFamilies #-}

import Control.Monad
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxing as VUG
import qualified Data.Vector.Unboxing.Mutable as VUGM

main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine
  lrs <- replicateM k $ do
    [l, r] <- map read . words <$> getLine
    pure (l, r)
  print =<< solve n lrs

solve :: Int -> [(Int, Int)] -> IO ModInt
solve n lrs = do
  dp <- VUGM.replicate (n + 1) (0 :: ModInt)
  sdp <- VUGM.replicate (n + 1) (0 :: ModInt)
  VUGM.write dp 1 1
  VUGM.write sdp 1 1
  VU.forM_ (VU.enumFromTo 2 n) $ \i -> do
    forM_ lrs $ \(l, r) -> do
      let left = max 1 (i - r)
      let right = max 0 (i - l)
      sr <- VUGM.read sdp right
      sl <- VUGM.read sdp (left - 1)
      VUGM.modify dp (+(sr - sl)) i
    sdpi <- VUGM.read sdp (i - 1)
    dpi <- VUGM.read dp i
    VUGM.write sdp i (sdpi + dpi)
  VUGM.read dp n


-- Modulus

modulus = 998244353

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