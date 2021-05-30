main :: IO ()
main = do
  [a, b, c, d] <- map read . words <$> getLine
  putStrLn $ if solve a b c d then "Yes" else "No"

data Turn = Takahashi | Aoki deriving Eq

solve :: Int -> Int -> Int -> Int -> Bool
solve a b c d = go a c Takahashi
  where
    go a c turn
      | a <= 0 = False
      | c <= 0 = True
      | otherwise = if turn == Takahashi then go a (c - b) Aoki else go (a - d) c Takahashi