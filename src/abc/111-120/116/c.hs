import Control.Monad

main :: IO ()
main = do
  n <- readLn :: IO Int
  hs <- map read . words <$> getLine
  print $ solve 0 hs
  
solve :: Int -> [Int] -> Int
solve i hs = go hs
  where
    go :: [Int] -> Int
    go hs
      | null hs = 0
      | all (>0) hs = 1 + go (filter (>=0) $ map (\h -> h - 1) hs)
      | otherwise = go (takeWhile (/=0) hs) + go (tail $ dropWhile (/=0) hs)
