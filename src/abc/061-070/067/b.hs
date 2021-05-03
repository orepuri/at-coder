import Data.List

main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine
  ls <- map read . words <$> getLine
  print $ sum $ take k $ sortBy (flip compare) ls