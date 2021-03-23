import Control.Monad

main :: IO ()
main = do
  [l, h] <- map read . words <$> getLine
  n <- readLn
  as <- replicateM n $ read <$> getLine
  forM_ (solve l h as) print

solve :: Int -> Int -> [Int] -> [Int]
solve l h = map (\a -> if a > h then -1 else if a >= l then 0 else l - a)