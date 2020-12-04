{-# LANGUAGE TypeApplications #-}

import Data.Maybe
import Text.Read

main :: IO ()
main = do
  (_,x) <- tuple2Int
  s <- getLine
  putStrLn $ show $ solve x s

solve :: Int -> String -> Int
solve score [] = score
solve score (x:xs) = case x of 'o' -> solve (score + 1) xs
                               'x' -> solve (max 0 (score - 1)) xs

tuple2Int :: IO (Int, Int)
tuple2Int = validateTuple2Int =<< readIntList

readIntList :: IO [Int]
readIntList = catMaybes . map readMaybeInt . words <$> getLine

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe @Int

validateTuple2Int :: [Int] -> IO (Int, Int)
validateTuple2Int [x,y] = pure (x,y)
validateTuple2Int _ = fail "Illegal arguments"
