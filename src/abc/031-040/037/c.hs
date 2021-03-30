import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import Data.Char

main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine :: IO [Int]
  as <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve n k as

solve :: Int -> Int -> VU.Vector Int -> Int
solve n k as = fst $ VU.ifoldl' (\(acc,p) i a -> let c = p - (as VU.! i) + a
                in (acc+c,c)) (init,init) tails
  where
    (head, tails) = VU.splitAt k as
    init = VU.sum head

