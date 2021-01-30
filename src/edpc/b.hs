import qualified Data.ByteString.Char8 as C
import Data.Char
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine :: IO [Int]
  hs <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print =<< solve n k hs


solve :: Int -> Int -> VU.Vector Int -> IO Int
solve n k hs = do
  dp <- VUM.replicate (n + 1) (maxBound :: Int)
  VUM.write dp 1 0
  VU.forM_ (VU.enumFromTo 1 (n - 1)) $ \i -> do
    let h1 = hs VU.! (i - 1)
    dp1 <- VUM.read dp i
    VU.forM_ (VU.enumFromTo (i + 1) (i + k)) $ \j -> do
      if j <= n then do
        let h2 = hs VU.! (j - 1)
        dp2 <- VUM.read dp j
        VUM.write dp j $ min dp2 $ dp1 + abs (h1 - h2)
      else pure ()
  VUM.read dp n

