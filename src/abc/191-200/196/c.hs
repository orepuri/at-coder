import Control.Monad
import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.Maybe

main :: IO ()
main = do
  n <- readLn
  print $ solve n

solve :: Int -> Int
solve n = length $ takeWhile (<=n) $ map (read . concat . replicate 2 . show) [1..]
