main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- words <$> getLine
  putStrLn $ if "Y" `elem` ss then "Four" else "Three"
