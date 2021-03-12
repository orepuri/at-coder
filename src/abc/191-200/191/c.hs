import Control.Monad
import Data.Maybe
import Data.Array
import qualified Data.ByteString.Char8 as C
 
main :: IO ()
main = do
  [h, w] <- map read . words <$> getLine :: IO [Int]
  mat <- array ((1, 1), (h, w)) . concat <$> forM [1..h] (\i -> fmap (zip (zip (repeat i) [1..w]) . C.unpack) C.getLine)
  let isVertex xs = odd $ length $ filter (== '#') xs
      ans = length $ filter id [isVertex [mat ! (i, j), mat ! (i - 1, j), mat ! (i, j - 1), mat ! (i - 1, j - 1)] | i <- [2..h], j <- [2..w]]
  print ans
