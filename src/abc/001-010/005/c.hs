import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Maybe

main :: IO ()
main = do
  t <- toInt <$> BS.getLine
  _ <- toInt <$> BS.getLine
  as <- readIntList <$> BS.getLine
  _ <- toInt <$> BS.getLine
  bs <- readIntList <$> BS.getLine
  putStrLn $ if solve t as bs then "yes" else "no"

solve :: Int -> [Int] -> [Int] -> Bool
solve _ _ [] = True
solve _ [] (_:_) = False
solve time (t:ts) (c:cs) =
  (t <= c) &&
    if c - t <= time then solve time ts cs
    else solve time ts (c:cs)

readIntList :: BS.ByteString -> [Int]
readIntList = map toInt . C.words

toInt :: BS.ByteString -> Int
toInt = fst . fromJust . C.readInt
