import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Maybe

main :: IO ()
main = do
  [n, w] <- readIntList
  print $ n `div` w

readIntList :: IO [Int]
readIntList = map toInt . C.words <$> BS.getLine

toInt :: BS.ByteString -> Int
toInt = fst . fromJust . C.readInt