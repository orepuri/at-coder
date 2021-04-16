import Data.List

main :: IO ()
main = do
  n <- readLn
  s <- getLine
  print $ solve n s

solve :: Int -> String -> Int
solve n s = maximum $ scanl' (\acc c -> if c == 'I' then acc + 1 else acc - 1) 0 s