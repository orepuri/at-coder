{-# LANGUAGE TypeApplications #-}

import Data.Maybe
import Text.Read

main :: IO ()
main = do
  (a,b) <- tuple2Int
  (c,d) <- tuple2Int
  putStrLn $ show $ (a * d) -  (b * c)

tuple2Int :: IO (Int, Int)
tuple2Int = validateTuple2Int =<< readIntList

readIntList :: IO [Int]
readIntList = catMaybes . map readMaybeInt . words <$> getLine

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe @Int

validateTuple2Int :: [Int] -> IO (Int, Int)
validateTuple2Int [x,y] = pure (x,y)
validateTuple2Int _ = fail "Illegal arguments"
