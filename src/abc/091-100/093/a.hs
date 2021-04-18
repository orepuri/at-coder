import Data.List

main :: IO ()
main = do
  s <- getLine
  putStrLn $ if sort s == "abc" then "Yes" else "No"