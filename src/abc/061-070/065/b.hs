import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import qualified Data.IntSet as S
import Data.Maybe

main :: IO ()
main = do
  n <- readLn
  as <- VU.replicateM n $ do
    fst . fromJust . C.readInt <$> C.getLine
  print $ solve as

solve :: VU.Vector Int -> Int
solve as = loop (as VU.! 0) S.empty 1
  where
    loop a bs acc
      | a == 2 = acc
      | S.member a bs = -1
      | otherwise = loop (as VU.! (a - 1)) (S.insert a bs) (acc + 1)
