
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import Numeric (showOct)


main :: IO ()
main = do
  n <- readLn :: IO Int
  print $ solve n

solve :: Int -> Int
solve n = length $ filter (not . isSeven) [1..n]

isSeven :: Int -> Bool
isSeven n = '7' `elem` decimalStr || '7' `elem` octalStr
  where
    decimalStr = show n
    octalStr = showOct n ""
  

readIntList :: IO [Int]
readIntList = map toInt . C.words <$> BS.getLine

toInt :: BS.ByteString -> Int
toInt = fst . fromJust . C.readInt