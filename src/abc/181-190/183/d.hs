{-# LANGUAGE TypeApplications #-}

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Array as A
import qualified Data.Array.IO      as IO
import qualified Data.Array.Unboxed as UA
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

main :: IO ()
main = do
  [n,w] <- readIntList
  putStrLn =<< (\r -> if r then "Yes" else "No") <$> solve n w

solve :: Int -> Int -> IO Bool
solve n w = do
  let max = 2*10^5
  table <- VUM.replicate (max+1) 0
  replicateM n $ do
    [s,t,p] <- readIntList
    VUM.modify table (+p) s
    VUM.modify table (+(-p)) t
  judge table 0 max w 0
  where
    judge table i max w acc =
      if i > max then return True
      else do
        v <- VUM.read table i
        let acc' = acc + v
        if acc' > w then return False
        else judge table (i+1) max w acc'

readInt :: IO Int
readInt = toInt <$> C.getLine

readIntList :: IO [Int]
readIntList = map toInt . C.words <$> C.getLine

readIntLists :: Int -> IO [[Int]]
readIntLists n = replicateM n readIntList


toInt :: BS.ByteString -> Int
toInt = fst . fromJust . C.readInt
