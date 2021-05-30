import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as Intro

import qualified Data.IntMap as M
import Control.Monad.Primitive
import Control.Monad
import Data.Char
import Data.List

main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  let freqByWord = VU.foldl' (\acc a -> M.insertWith (+) a 1 acc) M.empty as
      nums = M.size freqByWord
      diff = max 0 $ nums - k
      freqs = VU.fromList $ M.elems freqByWord :: VU.Vector Int
  sorted <- VU.thaw freqs
  Intro.sort sorted
  freezed <- VU.freeze sorted
  print $ VU.sum $ VU.take diff sorted

    


