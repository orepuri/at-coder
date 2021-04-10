main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  if a + b >= 10 then putStrLn "error"  else print $ a + b
