import Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Algorithms.Intro as Intro
import Data.Char
import Control.Monad.Primitive

main :: IO ()
main = do
  n <- readLn
  as <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  as' <- VU.thaw as
  Intro.sortBy (flip compare) as'
  as'' <- VU.freeze as'
  print $ solve (n-1) as''

solve :: Int -> VU.Vector Int -> Int
solve t as = go 0 t
  where
    go i t
      | t <= 0 = 0
      | i == 0 = as VU.! i + go (i+1) (t-1)
      | t >= 2 = as VU.! i * 2 + go (i+1) (t-2)
      | otherwise = as VU.! i + go (i+1) (t-1)