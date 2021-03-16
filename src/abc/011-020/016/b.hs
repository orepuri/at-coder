main :: IO ()
main = do
  [a, b, c] <- map read . words <$> getLine
  putStrLn $ if a + b == c
    then
      if a - b == c then "?" else "+"
    else
      if a - b == c then "-" else "!"