import Control.Monad
import Control.Monad.Primitive
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed as VU
import Data.Maybe

colors = 1000 * 1000 + 1

main :: IO ()
main = do
  n <- readLn
  imos <- VUM.replicate (colors+1) 0 :: IO (VUM.MVector (PrimState IO) Int)
  replicateM_ n $ do
    [a, b] <- map (fst . fromJust . C.readInt) . C.words <$> C.getLine
    VUM.modify imos (+1) a
    when (b + 1 <= colors) $ VUM.modify imos (subtract 1) (b + 1)
  VU.forM_ (VU.fromList [1..colors]) $ \i -> do
    p <- VUM.read imos (i - 1)
    VUM.modify imos (+p) i
  VU.freeze imos >>= print . VU.maximum
