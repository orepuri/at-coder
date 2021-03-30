import Control.Monad
import Data.Array.IO

main :: IO ()
main = do
  n <- readLn
  grid <- newArray ((1,1),(n,n)) ' ' :: IO (IOUArray (Int, Int) Char)
  forM_ [1..n] $ \i -> do
    s <- getLine
    forM_ (zip [1..n] s) $ \(j, c) -> do
      writeArray grid (i, j) c
  grid90 <- newArray ((1,1),(n,n)) ' ' :: IO (IOUArray (Int, Int) Char)
  forM_ [1..n] $ \i -> do
    forM_ [1..n] $ \j -> do
      c <- readArray grid (i, j)
      writeArray grid90 (j, n - i + 1) c
  forM_ [1..n] $ \i -> do
    forM_ [1..n] $ \j -> do
      c <- readArray grid90 (i, j)
      putChar c
    putChar '\n'
