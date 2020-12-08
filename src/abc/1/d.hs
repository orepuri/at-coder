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
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.IntMap as IM
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxing as VUG
import qualified Data.Vector.Unboxing.Mutable as VUGM
import Text.Printf

import Debug.Trace

main :: IO ()
main = do
  n <- readInt
  times <- sortBy (compare `on` fst) . unfoldr parseTime . BS.concat <$> replicateM n BS.getLine
  forM_ (merge times) (\(s,e) -> printf "%04d-%04d\n" s e)

merge :: [(Int,Int)] -> [(Int,Int)]
merge [] = []
merge [x] = [x]
merge (x:y:xs) = if isOverwrapping x y then merge $ (combine x y) : xs
              else x : merge (y:xs)

isOverwrapping :: (Int,Int) -> (Int, Int) -> Bool
isOverwrapping (_, xe) (ys, _) = xe >= ys

combine :: (Int, Int) -> (Int, Int) -> (Int, Int)
combine (xs, xe) (ys, ye) = (min xs ys, max xe ye)

roundTimes :: (Int, Int) -> (Int, Int)
roundTimes (s, e) = (roundStart s, roundEnd e)

roundStart :: Int -> Int
roundStart s = case s `mod` 5 of
  diff | diff == 0 -> s
       | otherwise -> s - diff

roundEnd :: Int -> Int
roundEnd e = case e `mod` 5 of
  diff | diff == 0 -> e
       | otherwise -> if e' `mod` 100 >= 60 then ((e `div` 100) + 1) * 100
                else e'
           where e' = e + (5 - diff)

parseTime :: Parser (Int, Int)
parseTime x = if BS.null x then Nothing
              else
                Just (times', rest)
  where
    (times, rest) = C.splitAt 9 x
    [s, e] = C.splitWith (== '-') times
    times' = roundTimes (toInt s, toInt e)

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
