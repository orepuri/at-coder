main :: IO ()
main = do
  s <- getLine
  print $ solve $ tail $ tail $ reverse s

solve :: String -> Int
solve s = length s - go s 0
  where
    go s i
      | l == r = i
      | otherwise = go (tail $ tail s) (i + 2)
      where
        n = length s `div` 2
        l = take n s
        r = drop n s
