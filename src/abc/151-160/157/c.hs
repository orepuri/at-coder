import Control.Monad
import Control.Monad.Primitive

import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.List

import Data.Maybe

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  scs <- replicateM m $ do
    [s, c] <- map (fst . fromJust . C.readInt) . C.words <$> C.getLine
    return (s, intToDigit c)
  print $ solve n scs

solve :: Int -> [(Int, Char)] -> Int
solve n scs = if null ok then -1 else head ok
  where
    ok = filter go [0..999]
    go i = n == l && all (\(s, c) -> l > (s - 1) && i' !! (s - 1) == c) scs
      where
        i' = show i
        l = length i'
