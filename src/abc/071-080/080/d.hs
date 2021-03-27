import Control.Monad
import Control.Monad.Primitive
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed as VU
import Data.Maybe
import qualified Data.Array.IO as IO

len :: Int
len = 10^5+1
--len = 100

main :: IO ()
main = do
  [n, c] <- map read . words <$> getLine
  times <- IO.newArray ((1,0), (c,len)) 0 :: IO (IO.IOUArray (Int, Int) Int) 
  replicateM_ n $ do
    [s, t, c] <- map (fst . fromJust . C.readInt) . C.words <$> C.getLine
    ps <- IO.readArray times (c, s)
    IO.writeArray times (c,s) (ps + 1) 
    when (t+1 <= len+1) $ do
      pt <- IO.readArray times (c, t+1)
      IO.writeArray times (c,t+1) (pt - 1) 
  let ls = VU.fromList [0..len-1]
  forM_ [1..c] $ \c' -> do
    VU.forM_ ls $ \i -> do
      p <- IO.readArray times (c', i)
      p' <- IO.readArray times (c', i + 1)
      IO.writeArray times (c', i + 1) (p + p') 
  cs <- VU.forM (VU.fromList [1..len-1]) $ \t -> do
    ts <- forM [1..c] $ \c' -> do
      t' <- IO.readArray times (c', t)
      return $ if t' > 0 then (1::Int) else 0
    return $ sum ts
  print $ VU.maximum cs

-- 0-13
-- 14

-- n = 3, c = 2
-- [0,0,0,0,0,0,0,0,0,0]
-- s = 1, t = 7
-- [0,0,0,0,0,0,0,0,0,0]
-- [1,0,0,0,0,0,0,0,-1,0,0]
-- s = 7, t = 8
-- [1,0,0,0,0,0,0,0,0,0,-1]
-- s = 8, t = 12
-- [1,0,0,0,0,0,0,0,0,0,0,0,0,-1]

-- 0 1 2 3 4 5 6 7 8 9 10 11 12
-- o x x x x x x x o o  o  o  o

-- o o o o o o o x o o  o  o  o
-- o x x x x x x x x o  o  o  o

-- o o o o o o o x x o  o  o  o
-- o x x x x x x x x x  x  x  x
