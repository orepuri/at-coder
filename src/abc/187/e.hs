import Control.Monad.Primitive
import Control.Monad
import qualified Data.Array.MArray
import qualified Data.Array.IO as AIO
import Data.Bits
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.List
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  n <- readLn :: IO Int
  graph <- AIO.newArray (1,n) [] :: IO (AIO.IOArray Int [Int])
  edges <- VUM.replicate n (0,0)
  forM_ [1..n-1] $ \i -> do
    [a,b] <- unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
    VUM.write edges i (a,b)
    bs <- AIO.readArray graph a
    AIO.writeArray graph a (b:bs)
    as <- AIO.readArray graph b
    AIO.writeArray graph b (a:as)

  depth <- VUM.replicate (n+1) (-1) :: IO (VUM.MVector (PrimState IO) Int)
  VUM.write depth 1 0
  buildDepth [1] graph depth

  q <- readLn :: IO Int
  scores <- VUM.replicate (n+1) 0

  replicateM_ q $ do
    [t,e,x] <- unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
    (a,b) <- VUM.read edges e
    depthA <- VUM.read depth a
    depthB <- VUM.read depth b
    let (a', b', t') = if depthA > depthB then (b, a, t `xor` 3)
                      else (a, b, t)
    if t' == 1 then do
      VUM.modify scores (+x) 1
      VUM.modify scores (subtract x) b'
    else do
      VUM.modify scores (+x) b'

  infiltrateScores [1] graph depth scores
  
  forM_ [1..n] $ \i -> do
    s <- VUM.read scores i
    print s
  
buildDepth :: [Int] -> AIO.IOArray Int [Int] -> VUM.MVector (PrimState IO) Int -> IO ()
buildDepth [] _ _ = pure ()
buildDepth (n:ns) graph depth = do
  children <- AIO.readArray graph n
  forM_ children $ \child -> do
    childDepth <- VUM.read depth child
    if childDepth == -1 then do
      parentDepth <- VUM.read depth n
      VUM.write depth child (parentDepth+1)
      buildDepth (child:ns) graph depth
    else buildDepth ns graph depth


infiltrateScores :: [Int] -> AIO.IOArray Int [Int] -> VUM.MVector (PrimState IO) Int -> VUM.MVector (PrimState IO) Int -> IO ()
infiltrateScores [] _ _ _ = pure ()
infiltrateScores (n:ns) graph depth scores = do
  children <- AIO.readArray graph n
  forM_ children $ \child -> do
    childDepth <- VUM.read depth child
    parentDepth <- VUM.read depth n
    if parentDepth < childDepth then do
      parentScore <- VUM.read scores n
      childScore <- VUM.read scores child
      VUM.write scores child (parentScore+childScore)
      infiltrateScores (child:ns) graph depth scores
    else infiltrateScores ns graph depth scores

