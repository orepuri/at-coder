import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Char

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  sums <- VUM.replicate (n + 1) 0
  maxs <- VUM.replicate (n + 1) 0
  VU.forM_ (VU.enumFromN 0 n) $ \i -> do
    prev <- VUM.read sums i
    let sum = prev + as VU.! i
    VUM.write sums (i + 1) sum
    prevMax <- VUM.read maxs i
    VUM.write maxs (i + 1) $ max prevMax sum
  sums' <- VU.freeze sums
  maxs' <- VU.freeze maxs
  print $ solve n sums' maxs'

solve :: Int -> VU.Vector Int -> VU.Vector Int -> Int
solve n sums maxs = VU.maximum $ VU.zipWith (+) positions maxs
  where positions = VU.scanl' (+) 0 sums
