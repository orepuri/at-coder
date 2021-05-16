import Control.Monad

main :: IO ()
main = do
  s <- getLine
  print $ solve s

solve :: String -> Int
solve s
  | length os > 4 = 0
  | otherwise = length $ filter go alls
  where
    alls = replicateM 4 [0..9]
    os = map fst $ filter (\(_, c) -> c == 'o') $ zip [0..9] s
    us = map fst $ filter (\(_, c) -> c == '?') $ zip [0..9] s
    xs = map fst $ filter (\(_, c) -> c == 'x') $ zip [0..9] s
    go s = all (`elem` s) os && all (`notElem` s) xs
