import qualified Data.ByteString.Char8 as C
import Data.Char
import qualified Data.Vector.Unboxed as VU
import qualified Data.Set as S

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve as
    

solve :: VU.Vector Int -> Int
solve as = S.size $ VU.foldl' (\acc a -> S.insert (odd' a) acc) S.empty as
  where
    odd' a = if even a then odd' $ a `div` 2 else a
