import Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import Data.Char
import Data.List

main :: IO ()
main = do
  n <- readLn :: IO Int
  abs <- VU.replicateM n $ do
    [a, b] <- unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
    pure (a, b)
  print $ solve abs

solve :: VU.Vector (Int, Int) -> Int
solve abs = VU.sum
  $ VU.map (\(a, b) -> (b * (b + 1) - a * (a - 1)) `div` 2) abs
