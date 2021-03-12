import qualified Data.ByteString.Char8 as C
import Data.Maybe

main :: IO ()
main = do
  [s,t] <- readIntList
  print $ t - s + 1

readIntList :: IO [Int]
readIntList = map toInt . C.words <$> C.getLine

toInt :: C.ByteString -> Int
toInt = fst . fromJust . C.readInt