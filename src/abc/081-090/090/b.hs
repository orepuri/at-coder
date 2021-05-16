main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  print $ solve a b

solve :: Int -> Int -> Int
solve a b = length $ filter go [a..b]
  where
    go n = s == reverse s
      where
        s = show n