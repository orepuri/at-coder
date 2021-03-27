import Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed as VU
import Data.Maybe

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  remains <- VU.replicateM n $ do
    [l, r, s] <- map (fst . fromJust . C.readInt) . C.words <$> C.getLine
    return (l, r, s)
  imos <- VUM.replicate (m + 1) 0
  VU.forM_ remains $ \(l, r, s) -> do
    VUM.modify imos (+s) l
    when (r + 1 <= m) $ VUM.modify imos (subtract s) (r + 1)
  forM_ [0..m - 1] $ \i -> do
    p <- VUM.read imos i
    VUM.modify imos (+p) (i + 1)
  imos' <- VU.freeze imos
  print $ VU.foldl' (\acc (_, _, s) -> acc + s) 0 remains - VU.minimum (VU.tail imos')
  