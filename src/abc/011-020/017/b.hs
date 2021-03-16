main :: IO ()
main = do
  x <- getLine
  putStrLn $ if solve x then "YES" else "NO"

solve :: String -> Bool
solve x
  | null x = True
  | take 2 (reverse x) == "hc" = solve $ take (length x - 2) x
  | last x == 'o' = solve $ init x
  | last x == 'k' = solve $ init x
  | last x == 'u' = solve $ init x
  | otherwise = False