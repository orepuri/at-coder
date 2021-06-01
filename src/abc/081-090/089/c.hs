import Control.Monad
import qualified Data.Array.IO as IO
import qualified Data.Array.Unboxed as AU
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.List
import qualified Data.IntMap as IM
import qualified Data.Map.Strict as M
import qualified Data.IntSet as IS

import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- V.replicateM n C.getLine
  print $ solve n ss

solve :: Int -> V.Vector C.ByteString -> Int
solve n ss = sum $ map go2 patterns
  where
    patterns = comb 3 "MARCH"
    freqs = V.foldl' go M.empty ss
    go2 p = product $ map freq p
    freq c = fromMaybe 0 $ M.lookup c freqs
    go acc s = if c `elem` "MARCH"
      then
        M.insertWith (+) c 1 acc
      else
        acc
      where
        c = C.head s

comb :: Int -> [a] -> [[a]]
comb 0 xs = [[]]
comb _ [] = []
comb n (x:xs) = [x:y | y <- comb (n-1) xs] ++ comb n xs