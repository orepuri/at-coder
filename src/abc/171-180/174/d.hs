import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU

main :: IO ()
main = do
  n <- readLn
  cs <- VU.fromList . C.unpack <$> C.getLine
  print $ solve n cs

solve :: Int -> VU.Vector Char -> Int
solve n cs = let (_, _, ans) = VU.foldl' go (0, rrs, inf) cs in min ans rrs
  where
    rrs = VU.length $ VU.filter (=='R') cs
    lws = 0
    inf = maxBound :: Int
    go :: (Int, Int, Int) -> Char -> (Int, Int, Int)
    go (ws, rs, ans) c = if c == 'R'
      then (ws, rs-1, min ans $ max ws (rs-1))
      else (ws+1, rs, min ans $ max (ws+1) rs)


-- W RWWRWR R +1
-- R W WRW R +1
-- WR W + 1

-- WW RRR WWR RR

-- i = 0, |WWRR lws=0, rrs=2
-- i = 1  W|WRR lws=1, rrs=2
-- i = 2  WW|RR lws=2, rrs=2
-- i = 3  WWR|R lws=2, rrs=1
-- i = 4  WWRR| lws=2, rrs=0