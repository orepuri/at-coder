import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.Maybe

main :: IO ()
main = do
  n <- readLn :: IO Int
  cs <- map toInt <$> replicateM n C.getLine
  print $ solve cs

solve :: [Int] -> Double
solve cs = sum $ map (solve' cs) cs

solve' :: [Int] -> Int -> Double
solve' cs c = if even numDivisors then 0.5 * fromIntegral (numDivisors+2) / fromIntegral (numDivisors+1)
              else 0.5
  where
    divisors = filter (\c' -> c `mod` c' == 0) cs
    numDivisors = length divisors - 1


toInt :: C.ByteString -> Int
toInt = fst . fromJust . C.readInt