import Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed as VU
import Data.Maybe 

main :: IO ()
main = do
  [n, q] <- map read . words <$> getLine
  imos <- VUM.replicate (n+1) 0
  replicateM_ q $ do
    [l, r] <- map (fst . fromJust . C.readInt) . C.words <$> C.getLine
    VUM.modify imos (+1) l
    when (r + 1 <= n) $ VUM.modify imos (subtract 1) (r+1)
  forM_ [2..n] $ \i -> do
    p <- VUM.read imos (i-1)
    VUM.modify imos (+p) i
  imos' <- VU.freeze imos
  putStrLn $ solve imos'

solve :: VU.Vector Int -> String
solve imos = tail $ VU.toList $ VU.map (\i -> if odd i then '1' else '0') imos
