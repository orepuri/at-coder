import Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Mutable as VM
import Data.Maybe

main :: IO ()
main = do
  n <- readLn
  tree <- VM.replicate n []
  replicateM_ (n - 1) $ do
    [a, b, c] <- map (fst . fromJust . C.readInt) . C.words <$> C.getLine
    VM.modify tree ((b-1, c):) (a-1)
    VM.modify tree ((a-1, c):) (b-1)
  tree' <- V.freeze tree
  [q, k] <- map read . words <$> getLine
  let costs :: VU.Vector Int
      costs = VU.create $ do
        vec <- VUM.replicate n 0
        let dfs current parent pcost = do
              let children = filter ((/= parent) . fst) (tree' V.! current)
              forM_ children $ \(child, cost) -> do
                VUM.write vec child (pcost + cost)
                dfs child current (pcost + cost)
        dfs (k - 1) (k - 1) 0
        return vec
  replicateM_ q $ do
    [x, y] <- map (fst . fromJust . C.readInt) . C.words <$> C.getLine
    print $ costs VU.! (x-1) + costs VU.! (y-1)