import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString.Char8 as C
import Data.Maybe

main :: IO ()
main = do
  [n, t] <- map read . words <$> getLine
  as <- VU.replicateM n $ do
    fst . fromJust . C.readInt <$> C.getLine
  print $ solve t as

solve :: Int -> VU.Vector Int -> Int
solve t as = let (ans, pending) = VU.foldl' loop (0, [VU.head as]) $ VU.tail as
  in ans + summary pending
  where
    loop (ans, []) a = (ans, [a])
    loop (ans, [p]) a = if a - p <= t then (ans, [p,a]) else (ans + t, [a])
    loop (ans, [p1,p2]) a = if a - p2 <= t then (ans, [p1,a]) else (summary [p1,p2] + ans, [a])
    summary [] = 0
    summary [p1] = t
    summary [p1,p2] = p2 + t - p1
