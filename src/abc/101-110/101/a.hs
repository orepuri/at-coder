main = do
  s <- getLine
  print $ foldr (\c acc -> if c == '+' then acc + 1 else acc - 1) 0 s