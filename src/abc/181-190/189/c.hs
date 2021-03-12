import Control.Monad
import Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as VU
import Data.Char

main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- map read . words <$> getLine
  print $ solve n $ zip [0..] as

solve :: Int -> [(Int, Int)] -> Int
solve n as = maximum $ loop as []
  where
    loop [] stack = [ h * (n - l) | (l, h) <- stack]
    loop (p:ps) [] = loop ps [p]
    loop (p:ps) qs = let (rs, smallers) = go p qs in rs ++ loop ps smallers
    go p@(l, h) qs =
      let (gtes, smallers) = span ((>= h) . snd) qs
      in if null gtes
        then ([], p:qs)
        else ([ h' * (l - l') | (l', h') <- gtes], (fst (last gtes), h):smallers)


-- 2 4 4 9 4 9

--       x   x
--       x   x
--       x   x
--       x   x
--   x x x x x
--   x x x x x
-- x x x x x x
-- x x x x x x 
-- s s e e e s

-- loop [(0,2),(1,4),(2,4),(3,9),(4,4),(5,9)] []

-- loop [(1,4),(2,4),(3,9),(4,4),(5,9)] [(0,2)]
-- go (1,4) [(0,2)]
-- rs = [], ts =[(1,4),(0,2)]

-- [] ++ loop [(2,4),(3,9),(4,4),(5,9)] [(1,4),(0,2)]
-- go (2,4) [(1,4),(0,2)]
-- rs = [(1,4)], ts = [(0,2)]
-- rs = [4], ts = [(1,4),(0,2)]

-- [4] ++ loop [(3,9),(4,4),(5,9)] [(1,4),(0,2)]
-- go (3,9) [(1,4),(0,2)]
-- rs = [], ts = [(3,9),(1,4),(0,2)]

-- [4] ++ loop [(4,4),(5,9)] [(3,9),(1,4),(0,2)]
-- go (4,4) [(3,9), (1,4),(0,2)]
-- rs = [9,12] ts = [(1,4),(0,2)])

-- [4,9,12] ++ loop [(5,9)] [(1,4),(0,2)])
-- go (5,9) [(1,4),(0,2)]
-- rs = [], ts = [(5,9),(1,4),(0,2)]

-- [4,9,18] ++ loop [] [(5,9),(1,4),(0,2)]
-- [9,20,12]