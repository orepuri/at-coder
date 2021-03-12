{-# LANGUAGE TypeApplications #-}

import Control.Monad
import Data.Maybe
import qualified Data.Array         as A
import qualified Data.Array.Unboxed as UA
import qualified Data.Array.IO      as IO

import Text.Read

main :: IO ()
main = do
  (a,b,c) <- tuple3Int
  putStrLn =<< show <$> solve a b c

solve :: Int -> Int -> Int -> IO Double
solve a b c = do
  dp <- IO.newArray ((a,b,c),(100,100,100)) 0.0 :: IO (IO.IOUArray (Int,Int,Int) Double)
  forM_ [99,98..a] $ \x -> do
    forM_ [99,98..b] $ \y -> do
      forM_ [99,98..c] $ \z -> do
        let [x',y',z'] = map fromIntegral [x,y,z]
        let t = x'+y'+z'
        ae <- IO.readArray dp (x+1,y,z)
        be <- IO.readArray dp (x,y+1,z)
        ce <- IO.readArray dp (x,y,z+1)
        let v = 1.0 + (x'/t) * ae + (y'/t) * be + (z'/t) * ce
        IO.writeArray dp (x,y,z) v
  IO.readArray dp (a,b,c)


tuple3Int :: IO (Int, Int, Int)
tuple3Int = validateTuple3Int =<< readIntList

validateTuple3Int :: [Int] -> IO (Int, Int, Int)
validateTuple3Int [x,y,z] = pure (x,y,z)
validateTuple3Int _ = fail "Illegal arguments"


readIntList :: IO [Int]
readIntList = catMaybes . map readMaybeInt . words <$> getLine

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe @Int

