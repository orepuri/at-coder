import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import Data.Maybe

main :: IO ()
main = do
  n <- readLn
  as <- VU.replicateM n $ do
    fst . fromJust . C.readInt <$> C.getLine
  print $ solve as

solve :: VU.Vector Int -> Int
solve as = undefined 