{-# LANGUAGE TypeApplications #-}

import Control.Monad
import Data.Maybe
import Data.Array.ST
import Data.Array.Unboxed

import Text.Read

main :: IO ()
main = do
  (a,b,c) <- tuple3Int
  putStrLn . show $ solve a b c

solve :: Int -> Int -> Int -> Double
solve a b c = (dp a b c) ! (a,b,c)

dp :: Int -> Int -> Int -> UArray (Int,Int,Int) Double
dp a b c = runSTUArray $ do
  dp <- newArray ((a,b,c),(100,100,100)) 0.0
  forM_ [99,98..a] $ \x -> do
    forM_ [99,98..b] $ \y -> do
      forM_ [99,98..c] $ \z -> do
        let [x',y',z'] = map fromIntegral [x,y,z]
        let t = x'+y'+z'
        ae <- readArray dp (x+1,y,z)
        be <- readArray dp (x,y+1,z)
        ce <- readArray dp (x,y,z+1)
        let v = 1.0 + (x'/t) * ae + (y'/t) * be + (z'/t) * ce
        writeArray dp (x,y,z) v
  return dp

tuple3Int :: IO (Int, Int, Int)
tuple3Int = validateTuple3Int =<< readIntList

validateTuple3Int :: [Int] -> IO (Int, Int, Int)
validateTuple3Int [x,y,z] = pure (x,y,z)
validateTuple3Int _ = fail "Illegal arguments"


readIntList :: IO [Int]
readIntList = catMaybes . map readMaybeInt . words <$> getLine

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe @Int

