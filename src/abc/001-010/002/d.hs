{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}

import Control.Monad
import Control.Monad.Primitive
import qualified Data.Array as A
import qualified Data.Array.IO as IO
import qualified Data.Array.Unboxed as UA
import Data.Bits
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
  [n,m] <- readIntList
  relations <- IO.newArray ((1,1),(n,n)) False :: IO (IO.IOUArray (Int, Int) Bool)
  pairs <- map (\[x, y] -> (x, y)) <$> readIntLists m
  forM_ pairs $ \p -> do
    IO.writeArray relations p True
  freezed <- IO.freeze relations
  putStrLn $ show $ solve n m freezed

solve :: Int -> Int -> UA.Array (Int, Int) Bool -> Int
solve n m relations =
  VU.maximum $ VU.map (\c -> members c n relations) $ VU.enumFromN 0 (2 ^ n)

members :: Int -> Int -> UA.Array (Int, Int) Bool -> Int
members c n relations =
  let pairs = toPairs c n
  in
    if all (\p -> relations UA.! p) pairs then popCount c
    else 0

toPairs :: Int -> Int -> [(Int, Int)]
toPairs x n = let xs = filter (testBit x) [0..n-1]
  in [(p1 + 1, p2 + 1) | p1 <- xs, p2 <- tail xs, p1 < p2]


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

parsePairWith :: Int -> Char -> (BS.ByteString -> BS.ByteString -> a) -> Parser a
parsePairWith n digit f str = if BS.null str then Nothing
              else
                Just (f x y, rest)
  where
    (head, rest) = C.splitAt n str
    [x, y] = C.splitWith (== digit) head

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
