main :: IO ()
main = do
  n <- readLn :: IO Int
  ws <- map read . words <$> getLine
  print $ solve ws

solve :: [Int] -> Int
solve ws = minimum 
  $ map (\(lw,rw) -> abs $ lw-rw)
  $ filter (\(lw,rw) -> lw /= 0 && rw /= 0)
  $ scanl (\(lw,rw) w -> (lw+w, rw-w)) (0,total) ws
  where
    total = sum ws
