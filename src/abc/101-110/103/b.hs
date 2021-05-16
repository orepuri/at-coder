main :: IO ()
main = do
  s <- getLine 
  t <- getLine
  putStrLn $ if solve s t then "Yes" else "No"

solve :: String -> String -> Bool
solve s t = any ((==t) . (\i -> let (l, r)  = splitAt i s in r ++ l)) [0..length s-1]