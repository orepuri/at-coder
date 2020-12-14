import qualified Data.ByteString.Char8 as C
import Data.Maybe

main :: IO ()
main = do
  n <- fst . fromJust . C.readInt <$> C.getLine
  putStrLn $ concat $ map show $ solve (n `mod` 30)

solve :: Int -> [Int]
solve n = foldl (\acc i -> swap (i `mod` 5) acc) [1,2,3,4,5,6] [0..n-1]

swap :: Int -> [Int] -> [Int]
swap 0 (x:y:xs) = y:x:xs
swap n (x:xs) = x : swap (n - 1) xs
