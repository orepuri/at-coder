main :: IO ()
main = do
  [a, b] <- words <$> getLine
  print $ 2 * read (a ++ b)
