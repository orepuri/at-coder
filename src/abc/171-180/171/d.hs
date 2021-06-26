import qualified Data.ByteString.Char8 as C
import Data.Char

import qualified Data.Vector.Unboxed as VU
import Data.Bits

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  putStrLn $ unwords . map show $ VU.toList $ solve n as

solve :: Int -> VU.Vector Int -> VU.Vector Int
solve n as = VU.map (xor all) as
  where
    all = VU.foldl1' xor as