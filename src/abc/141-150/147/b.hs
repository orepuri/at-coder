main :: IO ()
main = do
  s <- getLine
  print $ solve s

solve :: String -> Int
solve s = go l r' 0 
  where
    n = length s
    (l, r) = splitAt (n `div` 2) s
    r' = reverse $ if odd n then tail r else r
    go [] [] acc = acc
    go (l:ls) (r:rs) acc
      | l == r = go ls rs acc
      | otherwise =go ls rs (acc + 1)
      