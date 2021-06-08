import Control.Monad
import Control.Monad.Primitive
import qualified Data.ByteString.Char8 as C

import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VAI

main :: IO ()
main = do
  [n, gq] <- map read . words <$> getLine :: IO [Int]
  s <- C.getLine
  let vec :: VU.Vector Int
      vec = VU.scanl' (+) 0
            $ VU.generate (n - 1)
            $ \i -> if C.index s i == 'A' && C.index s (i + 1) == 'C' then 1 else 0
  forM_ [1..gq] $ \q -> do
    [l, r] <- map (fst . fromJust . C.readInt) . C.words <$> C.getLine
    print $ vec VU.! (r - 1) - vec VU.! (l - 1)

