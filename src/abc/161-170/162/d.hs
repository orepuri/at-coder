import Control.Monad
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
  n <- readLn :: IO Int
  s <- C.getLine
  print $ solve n s

solve :: Int -> C.ByteString -> Int
solve n s = r * g * b - ngs
  where
    r = C.length $ C.filter (=='R') s
    g = C.length $ C.filter (=='G') s
    b = C.length $ C.filter (=='B') s
    ngs = sum $ map (\i -> sum $ map (ng i) [i+1..n-2]) [0..n-3]
    ng i j
      | k > n - 1 = 0
      | ci /= cj && cj /= ck && ck /= ci = 1
      | otherwise = 0
        where
          k = j + (j - i)
          ci = C.index s i
          cj = C.index s j
          ck = C.index s k