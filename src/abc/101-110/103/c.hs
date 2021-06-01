import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve n as

solve :: Int -> VU.Vector Int -> Int
solve n as = VU.sum as - n
