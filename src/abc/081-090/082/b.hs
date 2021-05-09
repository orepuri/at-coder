import Data.List

main :: IO ()
main = do
  s <- getLine
  t <- getLine
  putStrLn $ if solve s t then "Yes" else "No"

solve :: String -> String -> Bool
solve s t = s' < t' 
  where
    s' = sort s
    t' = sortBy (flip compare) t