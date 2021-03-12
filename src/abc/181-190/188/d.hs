import Control.Monad
import Control.Monad.Primitive
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.Maybe
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Algorithms.Intro as VAIT

main :: IO ()
main = do
  [n,c] <- readIntList
  events <- VUM.replicate (n*2) (0,0)
  VU.forM_ (VU.enumFromN 0 n) $ \i -> do
    [a,b,ci] <- readIntList
    VUM.write events (2*i) (a-1, ci)
    VUM.write events (2*i+1) (b, negate ci) 
  VAIT.sortBy (\e1 e2 -> compare (fst e1) (fst e2)) events
  events' <- VU.freeze events
  let (ans, _, _) = VU.foldl (calc c) (0, 0, 0) events'
  print ans
  
calc :: Int -> (Int, Int, Int) -> (Int, Int) -> (Int, Int, Int)
calc primeCost (ans,currentCost,prevTime) (time,cost) =
  if time /= prevTime then
    let ans' = ans + min primeCost currentCost * (time - prevTime)
    in (ans', currentCost + cost, time)
  else 
    (ans, currentCost + cost, prevTime)

readIntList :: IO [Int]
readIntList = map toInt . C.words <$> C.getLine

toInt :: C.ByteString -> Int
toInt = fst . fromJust . C.readInt