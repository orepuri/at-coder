import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Maybe



main :: IO ()
main = do
  n <- toInt <$> BS.getLine
  ts <- map toInt <$> replicateM n BS.getLine
  print $ minimum ts  


readIntList :: BS.ByteString -> [Int]
readIntList = map toInt . C.words

toInt :: BS.ByteString -> Int
toInt = fst . fromJust . C.readInt
