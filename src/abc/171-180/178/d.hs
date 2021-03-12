import Control.Monad
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

main :: IO ()
main = do
  s <- readLn
  print =<< if s < 3 then pure 0 else solve s

modulus :: Int
modulus = 10^9 + 7

solve :: Int -> IO Int
solve s = do
  dp <- VUM.replicate s 1
  VU.forM_ (VU.enumFromTo 5 (s - 1)) $ \i -> do
    n1 <- VUM.read dp (i - 1)
    n3 <- VUM.read dp (i - 3)
    VUM.write dp i $ (n1 + n3) `mod` modulus
  VUM.read dp (s - 1)
