import Data.List

main :: IO ()
main = do
  n <- readLn
  s <- getLine
  print $ solve n s

solve :: Int -> String -> Int
solve n s = maximum $ map go [1..n-1]
  where
    go i = length $ filter (`elem` r') l'
      where
        (l, r) = splitAt i s
        l' = nub l
        r' = nub r
        