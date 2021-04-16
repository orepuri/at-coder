main :: IO ()
main = do
  [x, y] <- words <$> getLine :: IO [String]
  putStrLn $ if x == y then "=" else if x > y then ">" else "<"