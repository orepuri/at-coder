import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import qualified Data.IntMap as M

import Data.Char

main :: IO ()
main = do
  n <- readLn
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve as

solve :: VU.Vector Int -> Int
solve as = VU.sum $ VU.map (\(a, i) -> if i == a then 0 else if i > a then i - a else i) $ VU.fromList $ M.toList freqs
  where
    freqs = VU.foldl' (\acc a -> M.insertWith (+) a 1 acc) M.empty as

