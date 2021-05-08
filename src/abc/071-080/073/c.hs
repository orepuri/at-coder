import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import qualified Data.IntSet as S

import Data.Char
import Data.Maybe

main :: IO ()
main = do
  n <- readLn
  as <- VU.replicateM n $ do
    fst . fromJust . C.readInt <$> C.getLine
  print $ solve as

solve :: VU.Vector Int -> Int
solve as = S.size $ VU.foldl' go S.empty as
  where
    go acc a
      | S.member a acc = S.delete a acc
      | otherwise = S.insert a acc