import Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Maybe

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  imos <- VUM.replicate (n + 1) 0
  forM_ [1..m] $ \_ -> do
    [l, r] <- map (fst . fromJust . C.readInt) . C.words <$> C.getLine
    VUM.modify imos (+1) l
    when (r+1 <= n) $ VUM.modify imos (subtract 1) (r + 1)
  forM_ [0..n-1] $ \i -> do
    p <- VUM.read imos i
    VUM.modify imos (+p) (i + 1)
  imos' <- VU.freeze imos
  print $ VU.length $ VU.filter (==m) imos'
  