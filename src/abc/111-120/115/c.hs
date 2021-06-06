import Control.Monad
import qualified Data.ByteString.Char8 as C

import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VAI

main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine
  hs <- VU.replicateM n $ fst . fromJust . C.readInt <$> C.getLine
  hs' <- VU.thaw hs
  VAI.sort hs'
  print . solve n k =<< VU.freeze hs'

solve :: Int -> Int -> VU.Vector Int -> Int
solve n k hs = VU.minimum $ VU.zipWith (-) (VU.drop (k-1) hs) hs

