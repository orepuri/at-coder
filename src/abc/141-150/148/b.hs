main :: IO ()
main = do
  n <- readLn :: IO Int
  [s, t] <- words <$> getLine
  putStrLn $ solve s t

solve [] [] = []
solve (s:ss) (t:ts) = s : t : solve ss ts