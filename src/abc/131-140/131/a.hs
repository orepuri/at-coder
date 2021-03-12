main :: IO ()
main = do
  s <- getLine
  putStrLn $ if solve s then "Bad" else "Good"

solve :: String -> Bool
solve s = or $ zipWith (==) s (tail s)
