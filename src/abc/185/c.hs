import Data.List

main :: IO ()
main = do
  l <- readLn :: IO Integer
  print $ product [(l - 11)..(l - 1)] `div` product [1..11]

