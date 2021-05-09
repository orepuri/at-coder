import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Algorithms.Intro as Intro
import Data.Char
import Data.List

main :: IO ()
main = do
  n <- readLn
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  as' <- VU.thaw $ VU.map (`mod` 200) as
  Intro.sort as'
  as'' <- VU.freeze as'
  print $ solve n $ VU.toList as''

solve :: Int -> [Int] -> Int
solve n as = sum $ map (comb2 . length) $ group as
  where
    comb2 n = n * (n - 1) `div` 2
