import Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Maybe

main :: IO ()
main = do
  [n, k, q] <- map read . words <$> getLine
  imos <- VUM.replicate (n+1) (0::Int)
  replicateM_ q $ do
    a <- fst . fromJust . C.readInt <$> C.getLine
    VUM.modify imos (subtract 1) 0
    VUM.modify imos (+1) a
    when (a+1 <= n) $ VUM.modify imos (subtract 1) (a+1)
  forM_ [0..n-1] $ \i -> do
    p <- VUM.read imos i
    VUM.modify imos (+p) (i+1)
  forM_ [1..n] $ \i -> do
    VUM.modify imos (+k) i
  imos' <- VU.freeze imos
  VU.forM_ (VU.tail imos') $ \p -> do
    putStrLn $ if p > 0 then "Yes" else "No"


-- n=6, k=5, q=4

-- 3/1/3/2

-- -1 -1  0 -1 -1 -1
-- -1 -2 -1 -2 -2 -2
-- -2 -3 -1 -3 -3 -3
-- -3 -3 -2 -4 -4 -4

