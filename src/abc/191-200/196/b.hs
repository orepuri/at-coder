main :: IO ()
main = do
  x <- getLine
  putStrLn $ takeWhile (/='.') x