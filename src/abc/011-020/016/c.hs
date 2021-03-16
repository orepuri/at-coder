import Control.Monad
import Data.Array.IO
import Data.List

main :: IO ()
main = do
  [n, m] <- map read . words <$> getLine
  friends <- newArray ((1,1), (n,n)) False :: IO (IOUArray (Int, Int) Bool)
  replicateM_ m $ do
    [a, b] <- map read . words <$> getLine
    writeArray friends (a, b) True
    writeArray friends (b, a) True
  forM_ [1..n] $ \i -> do
    foff <- fmap concat $ forM [1..n] $ \j -> do
      ijf <- readArray friends (i, j)
      if ijf then do
        forM [1..n] $ \k -> do
          jkf <- readArray friends (j, k)
          ikf <- readArray friends (i, k)
          if jkf && not ikf && i /= k then pure k else pure 0
      else pure []
    print $ length . nub $ filter (/= 0) foff
