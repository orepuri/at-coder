import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.List
import Data.Maybe

main :: IO ()
main = do
  n <- readLn :: IO Int
  ps <- replicateM n $ do
    [x,y] <- unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
    pure (x,y)
  print $ solve ps

solve :: [(Int, Int)] -> Int
solve ps = length $ [(x,y)| 
  x <- ps,
  y <- ps,
  x < y,
  let slope = fromIntegral (snd y - snd x) / fromIntegral (fst y - fst x),
  -1.0 <= slope && slope <= 1.0]

toInt :: C.ByteString -> Int
toInt = fst . fromJust . C.readInt