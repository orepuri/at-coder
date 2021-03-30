import Control.Monad
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

main :: IO ()
main = do
  [n, q] <- map read . words <$> getLine
  xs <- VUM.replicate (n+1) 0
  replicateM_ q $ do
    [l, r, t] <- map read . words <$> getLine
    forM_ [l..r] $ \i -> do
      VUM.write xs i t
  xs' <- VU.freeze xs
  VU.forM_ (VU.tail xs') $ \x -> do
    print x
    
  