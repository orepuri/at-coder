import Data.List
import Data.Maybe
import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  k <- readLn
  print $ solve k

solve :: Int -> Int
solve k = maybe (-1) (+1) ans
  where
    ans = VU.elemIndex 0 $ VU.scanl' (\x _ -> (x * 10 + 7) `mod` k) (7 `mod` k) $ VU.generate k (+1)
