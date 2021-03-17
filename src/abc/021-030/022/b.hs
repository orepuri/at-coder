import Control.Monad
import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import qualified Data.IntMap.Strict as M

main :: IO ()
main = do
  n <- readLn
  as <- VU.replicateM n $ do
    fst . fromJust . C.readInt <$> C.getLine
  print $ solve as

solve :: VU.Vector Int -> Int
solve as = M.foldl' (\acc i -> acc + i - 1) 0 $ VU.foldl' (\acc a -> M.insertWith (+) a 1 acc) M.empty as
