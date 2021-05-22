import Data.List

main :: IO ()
main = do
  n <- readLn :: IO Int
  hs <- map read . words <$> getLine
  print $ solve hs

solve :: [Int] -> Int
solve hs = length $ filter viewable $ tail $ inits hs
  where
    viewable hs = maximum hs == last hs
