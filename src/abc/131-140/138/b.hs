main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- map read . words <$> getLine
  print $ 1.0 / sum (map (1.0 /) as)