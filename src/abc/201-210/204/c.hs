  import Control.Monad
  import qualified Data.Array.IO as AIO
  import qualified Data.Array.Unboxed as AU
  import qualified Data.ByteString.Char8 as C
  import Data.Char
  import Data.List
  import qualified Data.IntMap as IM
  import qualified Data.IntSet as IS
  import qualified Data.Map as M

  import Data.Maybe
  import qualified Data.Vector.Unboxed as VU
  import qualified Data.Vector.Unboxed.Mutable as VUM
  import qualified Data.Vector.Algorithms.Intro as VAI

  main :: IO ()
  main = do
    [n, m] <- map read . words <$> getLine
    abs <- VU.replicateM m $ do
      [a, b] <- map (fst . fromJust . C.readInt) . C.words <$> C.getLine
      return (a, b)
    print $ solve n m abs

  solve :: Int -> Int -> VU.Vector (Int, Int) -> Int
  solve n m abs = n + sum (map goals [1..n])
    where
      loads = VU.foldl' (\acc (a, b) -> M.insertWith IS.union a (IS.singleton b) acc) M.empty abs
      goals i = go i 0 (findNexts i) (IS.singleton i)
      go :: Int -> Int -> IS.IntSet -> IS.IntSet -> Int
      go i acc nexts passed
        | IS.null nexts' = acc
        | otherwise = go n' (acc + 1) (nexts2' `IS.union` findNexts n') (IS.insert i passed)
        where
          nexts' = IS.filter (`IS.notMember` passed) nexts
          (n',nexts2') = IS.deleteFindMin nexts'
          
      findNexts i = fromMaybe IS.empty $ M.lookup i loads
