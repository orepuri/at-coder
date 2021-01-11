import qualified Data.ByteString.Char8 as C
import Data.Maybe

main :: IO ()
main = do
  [x,y] <- readIntList
  putStrLn $ if solve x y then "Yes" else "No"

solve :: Int -> Int -> Bool
solve x y = abs (x-y) < 3

readIntList :: IO [Int]
readIntList = map toInt . C.words <$> C.getLine

toInt :: C.ByteString -> Int
toInt = fst . fromJust . C.readInt