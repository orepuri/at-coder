import qualified Data.ByteString.Char8 as C
import Data.Char
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Algorithms.Intro as VAI
import Data.List

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine :: IO [Int]
  as <- unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  asu <- VU.thaw $ VU.fromList (0 : as ++ [n + 1])
  VAI.sort asu
  sorted <- VU.freeze asu
  print $ solve sorted

solve :: VU.Vector Int -> Int
solve as = if VU.null diffs then 0
  else VU.sum $ VU.map (\i -> (i + k - 1) `div` k) diffs
  where
    diffs = VU.filter (>0) $ VU.zipWith ((-) . pred) (VU.tail as) as
    k = VU.minimum diffs