import Data.Char

main :: IO ()
main = do
  x <- getChar
  print $ ord x - ord 'A' + 1