import Control.Monad

main :: IO ()
main = do
  [h, w, k] <- map read . words <$> getLine
  grid <- replicateM h getLine
  print $ solve h w k grid

solve :: Int -> Int -> Int -> [String] -> Int
solve h w k grid = length $ filter id $ map f cands
  where
    f :: ([Int], [Int]) -> Bool
    f (rs, cs) = blacks == k
      where
        grid' = map snd $ filter (\(i,_) -> i `notElem` rs) $ zip [1..] grid
        remains = map (map snd . filter (\(c,_) -> c `notElem` cs) . zip [1..]) grid'
        blacks = length $ filter (=='#') $ concat remains
    cands = [(rs, cs) |rs <- comb [1..h], cs <- comb [1..w]]

comb :: [a] -> [[a]]
comb [] = [[]]
comb (x:xs) = map (x:) (comb xs) ++ comb xs
