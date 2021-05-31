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
import qualified Data.Vector.Algorithms.Intro as Intro


main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine
  abs <- VU.replicateM n $ do
    [a, b] <- map read . words <$> getLine
    return (a, b)
  abs' <- VU.thaw abs
  Intro.sortBy (\(a1, _) (a2, _) -> compare a1 a2) abs'
  abs'' <- VU.freeze abs'
  print $ solve k abs''

solve :: Int -> VU.Vector (Int, Int) -> Int
solve = go 0
  where
    go i remains helps
      | remains == 0 = i
      | otherwise = go (i + remains) (help i to) ato
      where
        to = i + remains
        (mae, ato) = VU.span (\(a, _) -> a <= to) helps
        help from to = VU.sum $ VU.map snd mae
    
    

-- 2 3
-- 2 1
-- 5 10

-- (2,1) (5,10)

-- go 0 3 [(2,1) (5,10)] 
-- go 3 1         // to = 3