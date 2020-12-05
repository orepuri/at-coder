{-# LANGUAGE TypeApplications #-}

import Data.Maybe
import Text.Read

main :: IO ()
main = do
  (a,b,c) <- tuple3Int
  putStrLn $ show $ solve a b c

solve :: Int -> Int -> Int -> Double
solve a b c
  | a == 100 || b == 100 || c == 100 = 0.0
  | otherwise = (ap * ((solve (a+1) b c) + 1.0)) + (bp * ((solve a (b+1) c) + 1.0)) + (cp * ((solve a b (c+1)) + 1.0))
  where
    total = a + b + c
    ap = fromIntegral a / fromIntegral total
    bp = fromIntegral b / fromIntegral total
    cp = fromIntegral c / fromIntegral total


tuple3Int :: IO (Int, Int, Int)
tuple3Int = validateTuple3Int =<< readIntList

validateTuple3Int :: [Int] -> IO (Int, Int, Int)
validateTuple3Int [x,y,z] = pure (x,y,z)
validateTuple3Int _ = fail "Illegal arguments"


readIntList :: IO [Int]
readIntList = catMaybes . map readMaybeInt . words <$> getLine

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe @Int

