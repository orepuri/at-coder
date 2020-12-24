import Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector as V
import Data.Maybe ( fromJust )

main :: IO ()
main = do
  [r,c] <- readIntList
  [sy,sx] <- readIntList
  [gy,gx] <- readIntList
  stage <- V.fromList <$> replicateM r C.getLine
  print $ solve r c sy sx gy gx stage

solve :: Int -> Int -> Int -> Int -> Int -> Int -> V.Vector C.ByteString -> Int
solve r c sy sx gy gx stage = 1

readIntList :: IO [Int]
readIntList = map toInt . C.words <$> C.getLine

toInt :: C.ByteString -> Int
toInt = fst . fromJust . C.readInt