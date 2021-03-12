import Data.List

main :: IO ()
main = do
  [a, b, c] <- map read . words <$> getLine :: IO [Int]
  print $ sort [a, b, c] !! 1