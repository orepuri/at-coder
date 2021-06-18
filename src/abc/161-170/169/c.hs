main :: IO ()
main = do
  [a, b] <- words <$> getLine
  let (b1, b2) = span (/='.') b
      b1' :: Int
      b1' = read b1 * 100
      b2' :: Int
      b2' = read $ tail b2
      ans100 = show $ read a * (b1' + b2')
  putStrLn $ if length ans100 < 3 then "0" else take (length ans100 - 2) ans100

