import qualified Data.IntMap as M
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import Data.Char
import Data.List
import Data.Maybe

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve as

solve :: VU.Vector Int -> Int
solve as
  | length cands < 2 = 0
  | snd c1 >= 4 = fst c1 * fst c1
  | otherwise = fst c1 * fst c2
  where
    cands = filter (\(_, c) -> c >= 2) $ M.toDescList $ go as M.empty 
    c1 = head cands
    c2 = head $ tail cands
    go as acc
      | VU.null as = acc
      | otherwise = go (VU.tail as) (M.insertWith (+) (VU.head as) 1 acc)
