import qualified Data.ByteString.Char8 as C
import Data.Char
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Algorithms.Intro as VAI
import Data.List

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine :: IO [Int]
  as <- unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve $ sort as

solve :: [Int] -> Int
solve as = sum $ map (\i -> ceiling $ fromIntegral i / fromIntegral k) diffs
  where
    diffs = zipWith (\x y -> x - y - 1) (0:as) as
    k = minimum diffs