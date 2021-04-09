import Data.Char

main :: IO ()
main = do
  s <- getLine
  putStrLn $ solve s

solve :: String -> String
solve s = board $ go 0 $ base 0
  where
    go n base' = if s == base' then n else go (n + 1) $ base (n + 1)
    board 0 = "Do"
    board 1 = "Do"
    board 2 = "Re"
    board 3 = "Re"
    board 4 = "Mi"
    board 5 = "Fa"
    board 6 = "Fa"
    board 7 = "So"
    board 8 = "So"
    board 9 = "La"
    board 10 = "La"
    board 11 = "Si"
    board 12 = "Do"
    board 13 = "Do"
    board 14 = "Re"
    board 15 = "Re"
    board 16 = "Mi"
    board 17 = "Fa"
    board 18 = "Fa"
    board 19 = "So"
    base n = take 20 $ concat $ replicate 3 $ rotate n "WBWBWWBWBWBW"
    rotate n (s:ss) = if n == 0 then s:ss else rotate (n-1) $ ss ++ [s]
