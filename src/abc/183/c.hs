{-# LANGUAGE TypeApplications #-}

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
import Data.List
import Data.Maybe

main :: IO ()
main = do
  [n,k] <- readIntList
  routes <- readIntLists n
  putStrLn $ show $ solve n k routes

solve :: Int -> Int -> [[Int]] -> Int
solve n k routes =
  length $ filter (== k) $ map calcTime (permutations [1..n-1])
  where
    routes' = UA.listArray ((0,0),(n-1,n-1)) (concat routes) :: UA.UArray (Int,Int) Int
    calcTime xs = sum $ map (\(x,y) -> routes' UA.! (x,y)) path'
      where
        path  = 0 : xs ++ [0]
        path' = zip path $ tail path
    

readInt :: IO Int
readInt = toInt <$> C.getLine

readIntList :: IO [Int]
readIntList = map toInt . C.words <$> C.getLine

readIntLists :: Int -> IO [[Int]]
readIntLists n = replicateM n readIntList


toInt :: BS.ByteString -> Int
toInt = fst . fromJust . C.readInt
