import Data.List

main :: IO ()
main = do
  n <- readLn :: IO Int
  s <- getLine
  print $ solve s

solve :: String -> Int
solve = length . filter (\s -> take 3 s == "ABC") . tails