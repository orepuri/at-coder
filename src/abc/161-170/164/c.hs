import Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.IntSet as S
import qualified Data.Hashable as H

import Data.Maybe
import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- VU.replicateM n $ H.hash <$> C.getLine
  print $ solve n ss

solve :: Int -> VU.Vector Int -> Int
solve n ss = S.size $ VU.foldl' (flip S.insert) S.empty ss
