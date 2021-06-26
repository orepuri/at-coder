import Data.List

main :: IO ()
main = do
  [a, b, c] <- sortBy (flip compare)  . map read . words <$> getLine
  print $ a + b
