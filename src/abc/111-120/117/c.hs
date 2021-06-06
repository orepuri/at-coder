import Control.Monad
import qualified Data.ByteString.Char8 as C
import Data.Char

import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VAI

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  xs <- VU.unfoldrN m (C.readInt . C.dropWhile isSpace) <$> C.getLine
  if n >= m
    then print 0
    else do
      xs1 <- VU.thaw xs
      VAI.sort xs1
      xs2 <- VU.freeze xs1
      let ds = VU.zipWith (-) (VU.tail xs2) xs2
      ds1 <- VU.thaw ds
      VAI.sort ds1
      ds2 <- VU.freeze ds1
      print $ VU.sum $ VU.take (m - n) ds2


-- 初期地点以外はm-n地点を訪問する
-- 初期地点からの距離が短い順にm-n地点訪問すればいい
