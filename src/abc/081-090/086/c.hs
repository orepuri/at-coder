import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import Data.Char
import Data.Maybe

main :: IO ()
main = do
  n <- readLn
  plans <- VU.replicateM n $ do
    [t, x, y] <- map (fst . fromJust . C.readInt) . C.words <$> C.getLine
    return (t, x, y)
  putStrLn $ if solve plans then "Yes" else "No"

solve :: VU.Vector (Int, Int, Int) -> Bool
solve plans = go (0, 0, 0) plans
  where
    go current plans
      | VU.null plans = True
      | otherwise = reachable current next && go next (VU.tail plans)
      where
        next = VU.head plans
    reachable (t1, x1, y1) (t2, x2, y2) = dis <= t && even (dis + t)
      where
        dis = abs (x1 - x2) + abs (y1 - y2)
        t = t2 - t1
