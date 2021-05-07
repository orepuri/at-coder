main :: IO ()
main = do
  s <- getLine
  putStrLn $ solve s

solve :: String -> String
solve s = reverse $ go s 1 "" 
  where
    go [] _ acc = acc
    go (c:cs) i acc
      | odd i = go cs (i + 1) (c:acc)
      | otherwise = go cs (i + 1) acc