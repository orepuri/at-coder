import Data.List

main :: IO ()
main = do
  s <- getLine
  t <- getLine
  putStrLn $ if s `isPrefixOf` t then "Yes" else "No"