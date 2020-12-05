{-# LANGUAGE TypeApplications #-}
 
import Data.Maybe
import qualified Data.Array as A
import qualified Data.Array.Unboxed as UA
 
import Debug.Trace
 
import Text.Read
 
main :: IO ()
main = do
  (a,b,c) <- tuple3Int
  putStrLn $ show $ solve a b c
 
solve :: Int -> Int -> Int -> Double
solve a b c = dp A.! (a,b,c)
  where
    dp = A.listArray ((a,b,c),(100,100,100)) $ map f [(x,y,z) | x <- [a..100], y <- [b..100], z <- [c..100]]
    f (x,y,z)
      | x == 100 || y == 100 || z == 100 = 0.0
      | otherwise =
        1.0 + (x'/t) * dp A.! (x+1,y,z) + (y'/t) * dp A.! (x,y+1,z) + (z'/t) * dp A.! (x,y,z+1)
      where
        [x',y',z'] = map fromIntegral [x,y,z]
        t = x'+y'+z'
 
 
tuple3Int :: IO (Int, Int, Int)
tuple3Int = validateTuple3Int =<< readIntList
 
validateTuple3Int :: [Int] -> IO (Int, Int, Int)
validateTuple3Int [x,y,z] = pure (x,y,z)
validateTuple3Int _ = fail "Illegal arguments"
 
 
readIntList :: IO [Int]
readIntList = catMaybes . map readMaybeInt . words <$> getLine
 
readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe @Int
