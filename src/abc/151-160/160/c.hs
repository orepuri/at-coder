import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.Char

import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  [k, n] <- map read . words <$> getLine
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve n k as

solve :: Int -> Int -> VU.Vector Int -> Int
solve n k as = k - max lastD md
  where
    lastD = k - VU.last as + VU.head as
    md = VU.maximum $ VU.zipWith (flip (-)) as (VU.tail as)
    
