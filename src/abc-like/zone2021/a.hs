main :: IO ()
main = do
  s <- getLine
  print  $ length . filter id $ map (\i -> "ZONe" == take 4 (drop i s)) [0..8]