import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import Data.Char

import Data.List

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve as

solve :: VU.Vector Int -> Int
solve as = VU.minimum
  $ VU.map (\(a, b) -> abs (a - b))
  $ VU.init
  $ VU.tail
  $ VU.scanl' (\(as, bs) a -> (as + a, bs - a)) (0, VU.sum as) as
