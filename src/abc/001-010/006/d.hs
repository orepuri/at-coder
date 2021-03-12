import Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Vector.Algorithms.Search
import Data.Maybe

inf :: Int
inf = maxBound :: Int

main :: IO ()
main = do
  n <- readLn :: IO Int
  cs <- VU.fromList . map toInt <$> replicateM n C.getLine 
  dp <- VUM.replicate n inf
  VU.forM_ (VU.enumFromTo 0 $ n-1) $ \i -> do
    if i == 0 then VUM.write dp i $ cs VU.! i
    else do
      idx <- binarySearchL dp $ cs VU.! i
      if idx <= i then VUM.write dp idx $ cs VU.! i
      else pure()
  len <- binarySearchL dp inf
  print $ n - len

toInt :: C.ByteString -> Int
toInt = fst . fromJust . C.readInt
