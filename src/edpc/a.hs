import qualified Data.ByteString.Char8 as C
import Data.Char
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

main :: IO ()
main = do
  n <- readLn
  hs <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print =<< solve n hs

solve :: Int -> VU.Vector Int -> IO Int
solve n hs = do
  dp <- VUM.replicate (n + 1) (maxBound :: Int)
  VUM.write dp 1 0
  VU.forM_ (VU.enumFromTo 1 (n - 1))$ \i -> do
    let p1 = hs VU.! (i - 1)
        p2 = hs VU.! i
    dp1 <- VUM.read dp i
    dp2 <- VUM.read dp (i + 1)
    VUM.write dp (i + 1) $ min dp2 $ dp1 + abs (p1 - p2)

    if i + 2 <= n then do
      let p3 = hs VU.! (i + 1)
      dp3 <- VUM.read dp (i + 2)
      VUM.write dp (i + 2) $ min dp3 $ dp1 + abs (p1 - p3)
    else pure ()
  VUM.read dp n
