import Control.Monad
import Control.Monad.Primitive

import qualified Data.ByteString.Char8 as C
import Data.Char

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Mutable as VM

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  hs <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  routes <- VM.replicate (n+1) [] :: IO (VM.MVector (PrimState IO) [Int])
  forM_ [1..m] $ \i -> do
    [a, b] <- map read . words <$> getLine
    VM.modify routes (a:) b
    VM.modify routes (b:) a
  routes' <- V.freeze routes
  print $ solve hs routes'

solve :: VU.Vector Int -> V.Vector [Int] -> Int
solve hs routes = VU.length $ VU.filter go $ VU.indexed hs
  where
    go :: (Int, Int) -> Bool
    go (i, h) = all (\i' -> h > hs VU.! (i' - 1)) (routes V.! (i + 1))
