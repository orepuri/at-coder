main :: IO ()
main = do
  s <- getLine
  t <- readLn
  print $ solve s t

solve :: String -> Int -> Int
solve s t = let (x,y) = go (0,0) s in if t == 1
    then abs x + abs y + unknown
    else
      if abs x + abs y - unknown > 0 then abs x + abs y - unknown
      else if odd (abs x + abs y - unknown) then 1 else 0
  where
    unknown = length $ filter (=='?') s
    go (x,y) [] = (x,y)
    go (x,y) ('L':ss) = go (x-1,y) ss
    go (x,y) ('R':ss) = go (x+1,y) ss
    go (x,y) ('U':ss) = go (x,y+1) ss
    go (x,y) ('D':ss) = go (x,y-1) ss
    go (x,y) (_:ss)   = go (x,y) ss

-- (1, 1) 
-- (1, -3) 1 + 3 - 5 = -1
-- (1, -3) + 5 -> 1
-- (100,-100) + 5 = 5 - 200
-- 