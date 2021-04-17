main :: IO ()
main = do
  s1 <- getLine
  s2 <- getLine
  s3 <- getLine
  putStrLn [head s1, s2 !! 1, s3 !! 2]