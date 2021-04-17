main :: IO ()
main = do
  [a, b, c, d] <- map read . words <$> getLine
  let l = a + b
      r = c + d
  putStrLn $ if l > r then "Left" else if r > l then "Right" else "Balanced"
