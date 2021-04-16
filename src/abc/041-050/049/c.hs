main :: IO ()
main = do
  s <- getLine
  putStrLn $ if solve s then "YES" else "NO"

solve :: String -> Bool
solve [] = True
solve ('d' : 'r' : 'e' : 'a' : 'm' : 'e' : 'r' : s) = solve s || solve ('e' : 'r' : s)
solve ('d' : 'r' : 'e' : 'a' : 'm' : s) = solve s
solve ('e' : 'r' : 'a' : 's' : 'e' : 'r' : s) = solve s
solve ('e' : 'r' : 'a' : 's' : 'e' : s) = solve s
solve _ = False
