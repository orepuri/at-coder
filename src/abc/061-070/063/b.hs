import Data.List

main :: IO ()
main = do
  s <- getLine
  let s' = sort s
  putStrLn $ if s' == nub s' then "yes" else "no"