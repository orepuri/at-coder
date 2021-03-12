{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Monad
import Control.Monad.Primitive
import qualified Data.Array as A
import qualified Data.Array.IO as IO
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.IntMap as IM
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxing as VUG
import qualified Data.Vector.Unboxing.Mutable as VUGM

main :: IO ()
main = do
  [deg, dis] <- readIntList
  let (dir, w) = solve (fromIntegral deg) (fromIntegral dis)
  putStrLn $ dir ++ " " ++ show w

solve :: Double -> Double -> (String, Int)
solve deg dis = (dir (deg / 10.0) dis, power dis)
  where
    dir deg dis
      | power dis == 0.0 = "C"
      | deg < 11.25  = "N"
      | deg < 33.75  = "NNE"
      | deg < 56.25  = "NE"
      | deg < 78.75  = "ENE"
      | deg < 101.25 = "E"
      | deg < 123.75 = "ESE"
      | deg < 146.25 = "SE"
      | deg < 168.75 = "SSE"
      | deg < 191.25 = "S"
      | deg < 213.75 = "SSW"
      | deg < 236.25 = "SW"
      | deg < 258.75 = "WSW"
      | deg < 281.25 = "W"
      | deg < 303.75 = "WNW"      
      | deg < 326.25 = "NW"
      | deg < 348.75 = "NNW"
      | otherwise = "N"
    power dis = case (dis / 60.0) of
      speed | speed < 0.25  -> 0
            | speed < 1.55  -> 1
            | speed < 3.35  -> 2
            | speed < 5.45  -> 3
            | speed < 7.95  -> 4
            | speed < 10.75  -> 5
            | speed < 13.85  -> 6
            | speed < 17.15  -> 7
            | speed < 20.75  -> 8
            | speed < 24.45  -> 9
            | speed < 28.45  -> 10
            | speed < 32.65  -> 11
            | otherwise  -> 12

-- Input

readInt :: IO Int
readInt = toInt <$> C.getLine

readIntList :: IO [Int]
readIntList = map toInt . C.words <$> C.getLine

readIntLists :: Int -> IO [[Int]]
readIntLists n = replicateM n readIntList

readIntVec :: IO (VU.Vector Int)
readIntVec = VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine

readCharMatrixAsVec :: Int -> Int -> IO (VU.Vector Char)
readCharMatrixAsVec r c = VU.unfoldrN (r*c) parseChar . BS.concat <$> replicateM r BS.getLine

toInt :: BS.ByteString -> Int
toInt = fst . fromJust . C.readInt

type Parser a = BS.ByteString -> Maybe (a, BS.ByteString)

parseChar :: Parser Char
parseChar = C.uncons

-- Modulus

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
