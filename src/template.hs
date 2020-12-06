{-# LANGUAGE TypeApplications #-}

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Array as A
import qualified Data.Array.IO as IO
import qualified Data.Array.Unboxed as UA
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed.Mutable as VUM

main :: IO ()
main = do
  putStrLn $ show $ solve

solve :: Int
solve = undefined


readInt :: IO Int
readInt = toInt <$> C.getLine

readIntList :: IO [Int]
readIntList = map toInt . C.words <$> C.getLine

readIntLists :: Int -> IO [[Int]]
readIntLists n = replicateM n readIntList


toInt :: BS.ByteString -> Int
toInt = fst . fromJust . C.readInt
