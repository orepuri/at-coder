import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import Data.Char

main :: IO ()
main = do
  [h, n] <- map read . words <$> getLine
  as <- VU.unfoldrN n (C.readInt . C.dropWhile isSpace) <$> C.getLine
  putStrLn $ if VU.sum as >= h then "Yes" else "No"