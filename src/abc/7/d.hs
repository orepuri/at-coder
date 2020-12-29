import Control.Applicative
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.Maybe


main :: IO ()
main = do
  [a,b] <- readIntList
  print $ solve (intToDigits b) - solve (intToDigits (a-1))

solve :: [Int] -> Int
solve [] = 0
solve (d:ds)
  | d < 4  = d * x + solve ds
  | d == 4 = 4 * x
  | d < 9  = (d-1) * x + 10^m + solve ds
  | d == 9 = (d-1) * x + 10^m + digitsToInt ds + 1
  where 
    m = length ds
    x = solve $ replicate m 9

intToDigits :: Int -> [Int]
intToDigits 0 = []
intToDigits n = map digitToInt (show n)

digitsToInt :: [Int] -> Int
digitsToInt [] = 0
digitsToInt ds = read $ map intToDigit ds

readIntList :: IO [Int]
readIntList = map toInt . C.words <$> C.getLine

toInt :: C.ByteString -> Int
toInt = fst . fromJust . C.readInt

