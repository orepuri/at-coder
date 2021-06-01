import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.Char

import Data.Maybe
import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ ceiling $ fromIntegral (n - 1) / fromIntegral (k - 1)
