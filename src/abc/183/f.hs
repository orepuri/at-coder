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
  [n,q] <- readIntList
  classes <- readIntVec
  classMap <- VM.replicate n M.empty
  V.forM_ (V.enumFromN 0 n) $ \i -> do
    VM.write classMap i $ M.singleton (classes VU.! i) 1
  fu <- VUM.replicate n 0
  V.forM_ (V.enumFromN 0 n) $ \i -> do
    VUM.write fu i i
  solve n q classMap fu

type ClassMap = VM.MVector (PrimState IO) (M.Map Int Int)
type FU = VUM.MVector (PrimState IO) Int

solve :: Int -> Int -> ClassMap -> FU -> IO ()
solve n q classMap fu = do
  V.forM_ (V.enumFromN 0 q) $ \i -> do
    input <- readIntList
    case input of 
      [1,a,b] -> do unite (a-1) (b-1) classMap fu
      [2,x,y] -> do query (x-1) y classMap fu

unite :: Int -> Int -> ClassMap -> FU -> IO ()
unite x y classMap fu = do
  leaderX <- getLeader x fu
  leaderY <- getLeader y fu
  if leaderX == leaderY then return ()
  else do
    xClasses <- VM.read classMap leaderX
    yClasses <- VM.read classMap leaderY
    let (nLeader, pLeader, bmap, lmap) = if M.size xClasses > M.size yClasses then (leaderX, leaderY, xClasses, yClasses) else (leaderY, leaderX, yClasses, xClasses)
    let updated = M.foldlWithKey (\acc key value ->  let newValue = value + M.findWithDefault 0 key bmap in M.insert key newValue acc) bmap lmap
    VM.write classMap nLeader updated
    VUM.write fu pLeader nLeader

getLeader :: Int -> FU -> IO Int
getLeader x fu = do
  rx <- VUM.read fu x
  if rx == x then return x
  else do
    rx' <- getLeader rx fu
    VUM.write fu x rx'
    return rx'

query :: Int -> Int -> ClassMap -> FU -> IO ()
query x y classMap fu = do
  leader <- getLeader x fu
  classes <- VM.read classMap leader
  print $ M.findWithDefault 0 y classes

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
