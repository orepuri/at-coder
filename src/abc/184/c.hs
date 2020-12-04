{-# LANGUAGE TypeApplications #-}

import Data.Maybe
import Text.Read

main :: IO ()
main = do
  (r1,c1) <- tuple2Int
  (r2,c2) <- tuple2Int
  putStrLn $ show $ solve (r1,c1) (r2,c2)

solve :: (Int,Int) -> (Int,Int) -> Int
solve (r1,c1) (r2,c2)
  | (r == 0) && (c == 0) = 0
  | (r == c) || (r == (-c)) = 1             -- A/B
  | abs(r) + abs(c) <= 3 = 1                -- C
  | (r + c) `mod` 2 == 0 = 2                -- A B
  | abs(r) + abs(c) <= 6 = 2                -- C C
  | abs ((r1 + c1) - (r2 + c2)) <= 3 = 2    -- A C
  | abs ((r1 - c1) - (r2 - c2)) <= 3 = 2    -- B C
  | otherwise = 3
  where r = r2 - r1
        c = c2 - c1
  
tuple2Int :: IO (Int, Int)
tuple2Int = validateTuple2Int =<< readIntList

readIntList :: IO [Int]
readIntList = catMaybes . map readMaybeInt . words <$> getLine

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe @Int

validateTuple2Int :: [Int] -> IO (Int, Int)
validateTuple2Int [x,y] = pure (x,y)
validateTuple2Int _ = fail "Illegal arguments"
