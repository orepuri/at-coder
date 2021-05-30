import Control.Monad
import qualified Data.Array.IO as IO
import qualified Data.Array.Unboxed as UA
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.List
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- map read . words <$> getLine
  bs <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  cs <- VU.replicateM n $ do
    [a, b, c] <- map read . words <$> getLine
    return (a, b, c)
  print $ solve n as
  putStrLn $ if isSatisfied cs then "Yes" else "No"

solve :: Int -> [Int] -> Int
solve n as = undefined

isSatisfied :: VU.Vector (Int, Int, Int) -> Bool
isSatisfied cs = undefined 