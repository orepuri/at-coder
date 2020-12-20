import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import Data.Maybe
import qualified Data.Vector.Algorithms.Intro as Intro

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.thaw . VU.fromList =<< readIntList
  Intro.sortBy (flip compare) as
  print . solve n =<< VU.freeze as

solve :: Int -> VU.Vector Int -> Int
solve n as = sum $ map f [1..n-1]
  where
    f = \i -> as VU.! (i - 1) * (n - i) - cs VU.! i
    cs = VU.scanr (+) 0 as

readIntList :: IO [Int]
readIntList = map toInt . C.words <$> BS.getLine

toInt :: BS.ByteString -> Int
toInt = fst . fromJust . C.readInt