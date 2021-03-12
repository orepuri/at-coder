main :: IO ()
main = do
  s <- getLine
  putStrLn $ map (\c -> if c == ',' then ' ' else c) s