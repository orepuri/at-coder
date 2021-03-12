import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import Data.Char

main :: IO ()
main = do
  n <- getLine
  as <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve as

solve :: VU.Vector Int -> Int
solve as = snd $ VU.foldl f (VU.head as, 0) $ VU.tail as
  where
    f (m, ans) a = if m <= a
      then (a, ans)
      else (m, ans + m - a) 
