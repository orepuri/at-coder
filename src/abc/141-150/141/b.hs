main :: IO ()
main = do
  s <- getLine
  putStrLn $ if solve s then "Yes" else "No"

solve :: String -> Bool
solve s = all (\(c, i) -> if odd i then c `elem` "RUD" else c `elem` "LUD") $ zip s [1..]