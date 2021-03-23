import Data.List

main :: IO ()
main = do
  s <- getLine
  k <- readLn
  print $ length . nub . sort $ map (\i -> take k (drop i s)) [0..length s - k]