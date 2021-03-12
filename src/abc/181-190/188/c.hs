import Control.Monad
import Control.Monad.Primitive
import qualified Data.ByteString.Char8 as C
import Data.Char
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print =<< solve (VU.indexed as)

solve :: VU.Vector (Int,Int) -> IO Int
solve as = if num == 2
  then do
    pure $ fst (VU.minimumBy (\p1 p2 -> compare (snd p1) (snd p2)) as) +1
  else do
    wins <- VUM.replicate (num `div` 2) (0,0) :: IO (VUM.MVector (PrimState IO) (Int, Int))
    VU.forM_  (VU.enumFromN 0 (num `div` 2)) $ \i -> do
      let p1 = as VU.! (2*i)
      let p2 = as VU.! (2*i+1)
      if snd p1 > snd p2 then
        VUM.write wins i p1
      else
        VUM.write wins i p2
    solve =<< VU.freeze wins
  where
    num = VU.length as