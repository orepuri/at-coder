import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Maybe


main :: IO ()
main = do
  [x,y] <- readIntList <$> BS.getLine
  print $ y `div` x

readIntList :: BS.ByteString -> [Int]
readIntList = map toInt . C.words

toInt :: BS.ByteString -> Int
toInt = fst . fromJust . C.readInt
