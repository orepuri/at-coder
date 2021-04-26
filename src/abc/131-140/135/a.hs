main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine
  if even $ a + b then
    print $ (a + b) `div` 2
  else putStrLn "IMPOSSIBLE"