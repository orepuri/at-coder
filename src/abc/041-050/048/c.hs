import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import Data.Char

main :: IO ()
main = do
  [n, x] <- map read . words <$> getLine
  as <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve x as

solve :: Int -> VU.Vector Int -> Int
solve x as = fst $ VU.foldl' go (init, a') $ VU.tail as
  where
    a = VU.head as
    (a', init) = if a > x then (x, a - x) else (a, 0)
    go (acc, pa) a = if pa + a > x
      then (acc + pa + a - x, x - pa)
      else (acc, a)

-- 各箱はx以下でなければならない
-- ai + aj <= x とする場合, ajを少なくする