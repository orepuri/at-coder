import Data.Char

main :: IO ()
main = do
  n <- readLn
  s <- getLine
  putStrLn $ solve n s

solve n = map rotate
  where
    rotate c = chr $ ((ord c - a) + n) `mod` 26 + a
    a = ord 'A'