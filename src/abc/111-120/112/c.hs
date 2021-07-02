{-# LANGUAGE BangPatterns #-}

import qualified Data.ByteString.Char8 as C
import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  n <- readLn :: IO Int
  xyhs <- VU.replicateM n $ do
    [x, y, h] <- map (fst . fromJust . C.readInt) . C.words <$> C.getLine
    return (x, y, h)
  let (cx, cy, ch) = solve n xyhs
  putStrLn $ unwords $ map show [cx, cy, ch]

solve :: Int -> VU.Vector (Int, Int, Int) -> (Int, Int, Int)
solve n xyhs = head $ filter (\c -> VU.all (go c) xyhs) cands
  where
    (bx, by, bh) = fromJust $ VU.find (\(_, _, h) -> h > 0) xyhs
    cands = [ cand (x, y) | x <- [0..100], y <- [0..100]]
    cand (x, y) = (x, y, bh + abs (bx - x) + abs (by - y))
    go (!cx, !cy, !ch) (x, y, h) = 
      h == max (ch - abs (x - cx) - abs (y - cy)) 0


-- h = max (ch - |x - cx| - |y - cy|), 0

-- h > 0
-- h = ch - |x - cx| - |y - cy|
-- ch = h + |x-cx|+|y-cy|

-- h == 0
-- ch - |x-cx| - |y-cy| <= 0
-- 1 < ch <= |x-cx| + |y-cy|