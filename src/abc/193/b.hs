import Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import Data.Char

main :: IO ()
main = do
  n <- readLn
  shops <- VU.replicateM n $ do
    s <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
    return (s VU.! 0, s VU.! 1, s VU.! 2)
  print $ solve shops

solve :: VU.Vector (Int, Int, Int) -> Int
solve shops = if VU.null costs then -1 else VU.minimum costs
  where
    costs = VU.filter (>0) $ VU.map f shops
    f (a, p, x) = if x - a > 0 then p else 0
