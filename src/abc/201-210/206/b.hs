import Data.List

main :: IO ()
main = do
  n <- readLn
  print $ length $ takeWhile (<n) $ scanl' (+) 0 [1..]

