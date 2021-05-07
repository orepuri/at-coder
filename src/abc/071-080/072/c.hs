import Control.Monad
import qualified Data.IntMap as M
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.List
import Data.Maybe

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- map read . words <$> getLine
--  let ans = VU.create $ do
  vec <- VUM.replicate (10^5+2) (0::Int)
  forM_ as $ \a -> do
    VUM.modify vec (+1) a
    VUM.modify vec (+1) (a+1)
    VUM.modify vec (+1) (a+2)
--    return vec
  ans <- VU.freeze vec
  print $ VU.maximum ans

solve :: [Int] -> Int
solve as = maximum $ map (\x -> freq (x-1) + freq x + freq (x+1)) xs
  where
    freqs = foldl' (\acc a -> M.insertWith (+) a 1 acc) M.empty as
    xs = nub $ sort $ concatMap (\a -> [a-1, a, a+1]) $ M.keys freqs
    freq x = fromMaybe 0 (M.lookup x freqs)


-- (x-1), x, (x+1)

-- 1 2
-- 2 1
-- 3 1
-- 4 1
-- 5 1
-- 9 1