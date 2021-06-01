import Control.Monad
import qualified Data.Array.IO as AIO
import qualified Data.Array.MArray
import qualified Data.Array.Unboxed as AU

import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.List
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VAI

main :: IO ()
main = do
  n <- readLn :: IO Int
  s <- VU.fromList . C.unpack <$> C.getLine
  east <- VUM.replicate n 0
  west <- VUM.replicate n 0
  VU.imapM_ (\i c -> do
    if c == 'W' then
      VUM.modify west (+1) i
    else
      VUM.modify east (+1) i) s
  forM_ [1..n-1] $ \i -> do
    x <- VUM.read west (i - 1)
    VUM.modify west (+x) i
    x <- VUM.read east (i - 1)
    VUM.modify east (+x) i
  east' <- VU.freeze east
  west' <- VU.freeze west
  print $ solve n east' west'

solve :: Int -> VU.Vector Int -> VU.Vector Int -> Int
solve n east west = VU.minimum $ VU.map go $ VU.generate n id
  where
    go i = ws i + es i 
      where
        ws i = if i == 0 then 0 else west VU.! (i - 1)
        es i = if i == (n-1) then 0 else VU.last east - east VU.! i
