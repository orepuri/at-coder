{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

import Control.Monad
import Control.Monad.Primitive
import qualified Data.Array as A
import qualified Data.Array.IO as IO
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.Function
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
import Debug.Trace
import Text.Printf

main :: IO ()
main = do
  [r, c] <- readIntegerList
  [x, y] <- readIntegerList
  [d, l] <- readIntegerList
  putStrLn $ show $ solve r c x y d l

solve :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer
solve r c x y d l = modInt2Int (p1 * p2 * p3)
  where
    p1 = int2ModInt $ (r - x + 1) * (c - y + 1)
    p2 = all - 2 * (noTop + noLeft) + (4 * noTopLeft + noTopBottom + noSide)
      - 2 * (onlyTop + onlyLeft) + allWrapped
    p3 = combMod dl (int2ModInt d)
    xy = x * y
    dl = int2ModInt $ d + l
    all = combModOrZero xy dl
    noTop = combModOrZero (xy - y) dl
    noLeft = combModOrZero (xy - x) dl
    -- r -> x, c -> y
    noTopLeft = combModOrZero (xy - x - y + 1) dl
    noTopBottom = combModOrZero (xy - 2 * x) dl
    noSide = combModOrZero (xy - 2 * y) dl
    onlyTop = combModOrZero (xy - x * 2 - y + 2) dl
    onlyLeft = combModOrZero (xy - y * 2 - x + 2) dl
    allWrapped = combModOrZero ((x - 2) * (y - 2)) dl
    combModOrZero x y =
      if x <= 0 then int2ModInt 0
      else combMod (int2ModInt x) y


-- Input

readInt :: IO Int
readInt = toInt <$> C.getLine

readIntList :: IO [Int]
readIntList = map toInt . C.words <$> C.getLine

readIntegerList :: IO [Integer]
readIntegerList = map toInteger <$> readIntList

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

parseIntTupleWith :: Int -> Char -> Parser (Int, Int)
parseIntTupleWith n digit str = if BS.null str then Nothing
              else
                Just (tuple, rest)
  where
    (head, rest) = C.splitAt n str
    [f, s] = C.splitWith (== digit) head
    tuple = (toInt f, toInt s)

-- Modulus

modulus = 10 ^ 9 + 7

newtype ModInt = ModInt Integer deriving (Eq, Ord)
 
--instance VUG.Unboxable ModInt where
--    type Rep ModInt = Integer
 
instance Num ModInt where
  (ModInt a) + (ModInt b) = ModInt $ mod (a + b) modulus
  (ModInt a) - (ModInt b) = ModInt $ mod (a - b) modulus
  (ModInt a) * (ModInt b) = ModInt $ mod (a * b) modulus
  fromInteger a = ModInt $ mod (fromInteger a) modulus
  abs (ModInt a) = undefined
  signum (ModInt a) = undefined
 
int2ModInt :: Integer -> ModInt
int2ModInt a = ModInt (mod a modulus)
 
modInt2Int :: ModInt -> Integer
modInt2Int (ModInt a) = a

instance Show ModInt where
  show (ModInt a) = show (mod a modulus)


-- Combination

combMod :: ModInt -> ModInt -> ModInt
combMod a b
  | a < b = 0
  | b < 0 = 0
  | b == 0 = 1
  | a == b = 1
combMod n k = factMod n * (invMod $ factMod k) * (invMod $ factMod (n - k))

factMod :: ModInt -> ModInt
factMod n = foldl1' (*) (int2ModInt <$> [1 .. (modInt2Int n)])

invMod :: ModInt -> ModInt
invMod n = powMod n $ int2ModInt (modulus - 2)

powMod :: ModInt -> ModInt -> ModInt
powMod _ 0 = 1
powMod b e = (powMod b $ int2ModInt (intE `div` 2)) ^ 2 * if intE `mod` 2 == 1 then b else 1
  where intE = modInt2Int e
