import Control.Monad

import qualified Data.ByteString.Char8 as C
import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VAI

main :: IO ()
main = do
  n <- readLn :: IO Int
  abs <- VU.replicateM n $ do
    [a, b] <- map (fst . fromJust . C.readInt) . C.words <$> C.getLine
    return (a, b)
  abs' <- VU.thaw abs
  VAI.sortBy (\(_, b1) (_, b2) -> compare b1 b2) abs'
  abs'' <- VU.freeze abs'
  putStrLn $ if solve n abs'' then "Yes" else "No"

solve :: Int -> VU.Vector (Int, Int) -> Bool
solve n abs = go 0 abs
  where
    go time abs
      | VU.null abs = True
      | time + a > b = False
      | otherwise = go (time + a) abs'
      where
        (a, b) = VU.head abs
        abs' = VU.tail abs
