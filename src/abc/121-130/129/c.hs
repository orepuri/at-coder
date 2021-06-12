import Control.Monad
import Control.Monad.Primitive

import qualified Data.ByteString.Char8 as C
import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VAI

modulus = 10^9+7

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  as' <- VU.replicateM m $ fst . fromJust . C.readInt <$> C.getLine
  as'' <- VUM.replicate (n+1) False :: IO (VUM.MVector (PrimState IO) Bool)
  VU.forM_ as' $ \a -> do
    VUM.write as'' a True
  broken <- VU.freeze as''
  dp <- VUM.replicate (n+1) 0 :: IO (VUM.MVector (PrimState IO) Int)
  VUM.write dp 0 1
  unless (broken VU.! 1) $ VUM.write dp 1 1
  forM_ [2..n] $ \i -> do
    unless (broken VU.! i) $ do
      p1 <- VUM.read dp (i-1)
      p2 <- VUM.read dp (i-2)
      VUM.write dp i $ (p1 + p2) `mod` modulus
  ans <- VUM.read dp n
  print ans

