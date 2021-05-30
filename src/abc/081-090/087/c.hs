import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import qualified Data.Array.Unboxed as AU
import qualified Data.Array.IO as AIO

import Data.Char
import Data.Maybe
import Control.Monad

main :: IO ()
main = do
  n <- readLn :: IO Int
  as1 <- map read . words <$> getLine :: IO [Int]
  as2 <- map read . words <$> getLine
  let grid = AU.listArray ((1,1), (2,n)) $ as1 ++ as2 :: AU.Array (Int, Int) Int
  candy <- AIO.newArray ((1,1),(2,n)) 0 :: IO (AIO.IOArray (Int, Int) Int)
  AIO.writeArray candy (2, n) $ grid AU.! (2,n)
  AIO.writeArray candy (1, n) (grid AU.! (2,n) + grid AU.! (1,n))

  forM_ [n-1,n-2..1] $ \w -> do
    lr <- AIO.readArray candy (2, w+1)
    ur <- AIO.readArray candy (1, w+1)

    let lc = grid AU.! (2,w) + lr
    AIO.writeArray candy (2, w) lc
    AIO.writeArray candy (1, w) $ grid AU.! (1,w) + max ur lc
      
  ans <- AIO.readArray candy (1,1)
  print ans
