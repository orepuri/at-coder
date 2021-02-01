import Control.Monad
import Control.Monad.Primitive
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Char
import Data.List

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  abs <- VU.replicateM m $ do
    [a, b] <- unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
    return (a, b)
  print =<< solve n abs

solve :: Int -> VU.Vector (Int, Int) -> IO Int
solve n abs = do
  uf <- VUM.replicate (n + 1) (-1)
  VUM.write uf 0 0
  VU.forM_ abs $ \(a, b) -> do
    rootA <- findRoot uf a
    rootB <- findRoot uf b
    if rootA /= rootB then do
      rootA' <- VUM.read uf rootA
      rootB' <- VUM.read uf rootB
      if rootA' < rootB' then do
        VUM.modify uf (+rootB') rootA
        VUM.write uf rootB rootA
      else do
        VUM.modify uf (+rootA') rootB
        VUM.write uf rootA rootB
    else pure ()
  negate . VU.minimum <$> VU.freeze uf
  where
    findRoot :: VUM.MVector (PrimState IO) Int -> Int -> IO Int
    findRoot uf x = do
      xv <- VUM.read uf x
      if xv < 0 then pure x else do
        xv' <- findRoot uf xv
        VUM.write uf x xv'
        pure xv'



