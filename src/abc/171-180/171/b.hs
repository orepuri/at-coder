import Data.List

main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine
  ps <- map read . words <$> getLine
  print $ sum $ take k $ sort ps