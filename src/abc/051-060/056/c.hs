main :: IO ()
main = do
  x <- readLn
  print $ solve x

solve :: Int -> Int
solve x = go 1
  where
    go t
      | t * (t + 1) `div` 2 >= x = t
      | otherwise = go (t + 1)
