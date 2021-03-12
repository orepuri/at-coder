import Control.Monad
import qualified Data.Vector.Unboxed as VU
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.List

main :: IO ()
main = do
  n <- readLn :: IO Int
  abs <- VU.replicateM n $ do
    [a, b] <- unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
    return (a, b)
  let (as, bs) = VU.unzip abs
  print $ solve n as bs

solve :: Int -> VU.Vector Int -> VU.Vector Int -> Int
solve n as bs = loop 0 0 maxBound 
  where
    loop i j ans 
      | i == n - 1 = if j == n - 1 then ans else loop i (j + 1) $ min ans (time i j)
      | otherwise = if j == n - 1
                  then loop (i + 1) 0 $ min ans (time i j)
                  else loop i (j + 1) $ min ans (time i j)
    time i j =
      if i == j
      then (as VU.! i) + (bs VU.! j)
      else max (as VU.! i) (bs VU.! j)
