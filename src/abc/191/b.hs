import qualified Data.ByteString.Char8 as C
import Data.Char
import qualified Data.Vector.Unboxed as VU
import Data.List

main :: IO ()
main = do
  [n, x] <- map read . words <$> getLine
  as <- unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  putStrLn $ unwords $ map show (filter (/= x) as)

