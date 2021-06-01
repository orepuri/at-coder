import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.Char

import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve n as

solve :: Int -> VU.Vector Int -> Int
solve n as = VU.sum $ VU.map (go 0) $ VU.filter even as

go acc n 
  | odd n = acc
  | otherwise = go (acc + 1) (n `div` 2)