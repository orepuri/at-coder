main :: IO ()
main = do
  [a, b, c] <- map read . words <$> getLine
  putStrLn $ if c == 0
    then if a > b then "Takahashi" else "Aoki"
    else if b > a then "Aoki" else "Takahashi"