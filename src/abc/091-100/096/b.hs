import Data.List

main :: IO ()
main = do
  [a, b, c] <- sortBy (flip compare) . map read . words <$> getLine
  k <- readLn
  print $ a * (2 ^ k) + b + c
  