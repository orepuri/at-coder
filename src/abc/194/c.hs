import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import Data.Char

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve n as

solve :: Int -> VU.Vector Int -> Int
solve n as = (n - 1) * y - 2 * x
  where
    x = snd $ VU.foldl' (\(accSum, accRes) a -> (accSum + a, a * accSum + accRes)) (VU.head as, 0) $ VU.tail as
    y = VU.sum $ VU.map (^2) as
