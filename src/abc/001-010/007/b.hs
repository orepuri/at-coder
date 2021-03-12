import Data.Char

main :: IO ()
main = do
  a <- getLine
  putStrLn $ solve a

solve :: String -> String
solve ['a'] = "-1"
solve ('a':as) = "a" 
solve (a:as) = chr (ord a - 1) : as
