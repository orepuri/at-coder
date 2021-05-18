import Data.List
import Data.Char

main :: IO ()
main = do
  s <- getLine
  putStrLn $ if solve s then "AC" else "WA"

solve :: String -> Bool
solve s = head s == 'A'
  && length (filter (=='C') $ init (drop 2 s)) == 1
  && all isLower (filter (\c -> c /= 'A' && c /= 'C') s)