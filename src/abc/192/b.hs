import Data.Char

main :: IO ()
main = do
  s <- getLine
  putStrLn $ if solve s then "Yes" else "No"

solve :: String -> Bool
solve s = length s == s'
  where s' = length $ filter (\(c,n) -> if odd n then isLower c else isUpper c) $ zip s [1..]