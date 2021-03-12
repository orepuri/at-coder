main :: IO ()
main = do
  s <- getLine
  print $ solve s

solve :: String -> Int
solve "RRR" = 3
solve "RRS" = 2
solve "RSR" = 1
solve "SRR" = 2
solve s = if 'R' `elem` s then 1 else 0
