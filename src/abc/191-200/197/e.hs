import Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import qualified Data.Set as S
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as Intro
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as M

main :: IO ()
main = do
  n <- readLn
  coordinates <- VU.replicateM n $ do
    [x, c] <- map (fst . fromJust . C.readInt) . C.words <$> C.getLine
    return (x, c)
  let rangeByColor = VU.foldr (\(x, c) m -> case M.lookup c m of
            Nothing -> M.insert c (x, x) m
            Just (l,r) -> M.insert c (min l x, max r x) m)
        M.empty coordinates
      ranges = VU.create $ do
        rng <- VUM.replicate (M.size rangeByColor + 1) (0, 0)
        forM_ (zip [0..] $ map snd $ M.toAscList rangeByColor) $ \(i, lr) -> do
          VUM.write rng i lr
        return rng
      (ans, _, _, _) = VU.foldl' f (0,0,0,0) ranges
  print ans

f :: (Int, Int, Int, Int) -> (Int, Int) -> (Int, Int, Int, Int)
f (plc, prc, pl, pr) (l, r) = (min l2l r2l, min l2r r2r, l, r)
  where
    l2l = plc + abs (pl - r) + (r - l)
    r2l = prc + abs (pr - r) + (r - l)
    l2r = plc + abs (pl - l) + (r - l)
    r2r = prc + abs (pr - l) + (r - l)
