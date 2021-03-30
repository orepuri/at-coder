import Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Data.Maybe

main :: IO ()
main = do
  n <- readLn
  as <- VU.replicateM n $ do
    fst . fromJust . C.readInt <$> C.getLine
  let patterns = M.fromList $ zip (S.toAscList $ VU.foldr S.insert S.empty as) [0..]
  VU.forM_ as $ \a -> do
    print $ fromJust (M.lookup a patterns)
