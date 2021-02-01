import Data.List
import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  n <- readLn
  print $ solve n

solve :: Int -> Int
solve n = (*) 2 $ VU.foldl' (\acc x -> acc + f x) 0 $ VU.enumFromTo 1 $ round (sqrt (fromIntegral n2))
  where
    n2 = n * 2
    f x | n2 `mod` x == 0 && even (abs (n2 `div` x - x + 1)) = 1
        | otherwise = 0
