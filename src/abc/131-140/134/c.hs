import Control.Monad

import qualified Data.ByteString.Char8 as C

import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VAI

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.replicateM n $ fst . fromJust . C.readInt <$> C.getLine
  as' <- VU.thaw as
  VAI.sortBy (flip compare) as'
  sorted <- VU.freeze as'
  let m1 = sorted VU.! 0
      m2 = sorted VU.! 1
  VU.forM_ as $ \a -> do
    print $ if a == m1 then m2 else m1

