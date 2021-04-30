import Data.List

main :: IO ()
main = do
  s <- getLine
  putStrLn $ if 1 == (length . nub) s then "No" else "Yes"