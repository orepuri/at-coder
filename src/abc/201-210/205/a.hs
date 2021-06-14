import Control.Monad

main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  print $ a / 100 * b
