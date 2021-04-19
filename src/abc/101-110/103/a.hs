import Data.List

main :: IO ()
main = do
  [a1, a2, a3] <- sort . map read . words <$> getLine :: IO [Int]
  print $ abs (a3 - a2) + abs (a2 - a1)