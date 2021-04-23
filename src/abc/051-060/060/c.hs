import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import Data.Char

main :: IO ()
main = do
  [n, t] <- map read . words <$> getLine
  ts <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve n t ts

solve :: Int -> Int -> VU.Vector Int -> Int
solve n t ts = n * t - loop (VU.head ts) (VU.tail ts) 0
  where
    loop t1 ts acc
      | VU.null ts = acc
      | otherwise = if t2 - t1 >= t then loop t2 ts' acc else loop t2 ts' $ acc + t - (t2 - t1)
      where
        t2 = VU.head ts
        ts' = VU.tail ts

