import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import Data.Char

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- VU.unfoldr (C.readInt . C.dropWhile isSpace) <$> C.getLine
  print $ solve n as

solve :: Int -> VU.Vector Int -> Int
solve n as = VU.maximum $ VU.imap (\ti _ -> let (ts, _, _) = aokiHand ti in ts) as
  where
    aokiHand :: Int -> (Int, Int, Int) -- (Tscore, Ascore, Aindex)
    aokiHand ti = VU.ifoldl' (\(ts', as', ai') ai a -> if ai == ti then (ts',as',ai') else let (ts, as) = scores ti ai in
      if as > as' then (ts, as, ai) else (ts', as', ai')) (minBound,minBound,-1) as

    scores :: Int -> Int -> (Int, Int)
    scores i1 i2 = if i1 == i2 then (minBound, minBound) else VU.ifoldl' (\(ta, aa) i a -> if even i then (ta + a, aa) else (ta, aa + a)) (0,0) $ VU.slice i1' (i2'-i1'+1) as
      where
        i1' = min i1 i2
        i2' = max i1 i2


-- Tの場所を決める
-- Aが最大になるAの場所を決める
-- Tの値を決める