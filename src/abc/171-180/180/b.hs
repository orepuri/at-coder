import qualified Data.ByteString.Char8 as C
import Data.Char
import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  n <- readLn :: IO Int
  xs <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  let (m, e, c) = solve xs
  print m
  print e
  print c

solve :: VU.Vector Int -> (Int, Double, Int)
solve xs = (manhattanDis, euclideanDis, chebyshevDis)
  where
    manhattanDis = VU.sum absVec
    euclideanDis = sqrt $ fromIntegral $ VU.sum $ VU.map (\x -> x * x) xs
    chebyshevDis = VU.maximum absVec
    absVec = VU.map abs xs