import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import Data.Char
import Data.Maybe
import qualified Data.IntMap as MI
import qualified Data.Map as M

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  bs <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  cs <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve as bs cs

solve :: VU.Vector Int -> VU.Vector Int -> VU.Vector Int -> Int
solve as bs cs = VU.sum $ VU.map (\a -> fromMaybe 0 $ MI.lookup a cmap) as
  where
    cv = VU.map (\c -> bs VU.! (c-1)) cs
    cmap = VU.foldl' (\acc c -> MI.insertWith (+) c 1 acc) MI.empty cv


-- 1 2 2
-- 3 1 2
-- 2 3 2

-- 2 3 2
-- 1 2 1

-- 1 2 2
-- 1 2 1


-- i = 1, a = 1
--   1->2, c=2

-- B値とindex
-- C値と件数


-- 1 2 1
-- 1 3 1
-- 2 3 2
-- 2 3 2

