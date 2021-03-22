main :: IO ()
main = do
  [a, op, b] <- words <$> getLine
  let a' = read a
      b' = read b
  print $ if op == "+" then a' + b' else a' - b'