import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import Data.Char

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine :: IO [Int]
  [x, y] <- map read . words <$> getLine
  as <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  bs <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve x y as bs

solve :: Int -> Int -> VU.Vector Int -> VU.Vector Int -> Int
solve x y as bs = go as bs 0 0
  where
    go :: VU.Vector Int -> VU.Vector Int -> Int -> Int -> Int
    go as bs boards time
      | inA && VU.null as = boards `div` 2
      | inB && VU.null bs = boards `div` 2
      | VU.null as && VU.null bs = boards `div` 2
      | inA = if time <= a
              then go (VU.tail as) bs (boards+1) (a+x)
              else go (VU.tail as) bs boards time
      | inB = if time <= b
              then go as (VU.tail bs) (boards+1) (b+y)
              else go as (VU.tail bs) boards time
      where
        inA = even boards
        inB = not inA
        a = VU.head as
        b = VU.head bs
