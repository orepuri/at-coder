main :: IO ()
main = do
  s <- getLine
  putStrLn $ solve s

solve :: String -> String
solve s = let (l, r) = go s "" "" in uniq (r ++ reverse l) ""
  where
    go [] l r = (l, r)
    go (c:cs) l r
      | c == 'R' = go cs r l
      | otherwise = if null l then go cs (c:l) r else if c == head l then go cs (tail l) r else go cs (c:l) r
    uniq [] ans = reverse ans
    uniq (c:cs) [] = uniq cs [c]
    uniq (c:cs) (t:ts)
      | c == t = uniq cs ts
      | otherwise = uniq cs (c:t:ts)
