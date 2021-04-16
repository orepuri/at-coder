import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Algorithms.Intro as Intro
import Data.Char

m:: Int
m = 10^9 + 7

main :: IO ()
main = do
  n <- readLn
  an <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  an' <- VU.thaw an
  Intro.sort an'
  an'' <- VU.freeze an'
  print $ solve n an''

solve :: Int -> VU.Vector Int -> Int
solve n an
  | odd n = if validOdd then modPow 2 ((VU.length an - 1) `div` 2) else 0
  | even n = if validEven then modPow 2 (VU.length an `div` 2) else 0
  where
    validOdd = VU.ifoldl' (\acc i a -> acc && (if even i then a == i + 2 else a == i + 1)) (VU.head an == 0) $ VU.tail an
    validEven = VU.ifoldl' (\acc i a -> acc && (if even i then a == i + 1 else a == i)) True an

modPow :: Int -> Int -> Int
modPow n 0 = 1
modPow n c
  | odd c = 2 * r `mod` m
  | otherwise = r
  where
    q = modPow 2 (c `div` 2)
    r = q * q `mod` m
