import Data.Char

main :: IO ()
main = do
  s <- getLine
  putStrLn $ toUpper (head s) : map toLower (tail s)