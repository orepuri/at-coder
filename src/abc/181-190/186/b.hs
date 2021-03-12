import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Maybe

main :: IO ()
main = do
  [h, w] <- readIntList
  blocks <- concat <$> replicateM h readIntList
  print $ solve blocks

solve :: [Int] -> Int
solve blocks = sum $ map (\b -> b - min) blocks
  where
    min = minimum blocks

readIntList :: IO [Int]
readIntList = map toInt . C.words <$> BS.getLine

toInt :: BS.ByteString -> Int
toInt = fst . fromJust . C.readInt