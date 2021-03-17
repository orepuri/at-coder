main :: IO ()
main = do
  s <- getLine
  putStrLn $ solve s

solve :: String -> String
solve s = concat $ go s
  where
    go [] = []
    go (s:ss) = (s : show (length x + 1)) : go y
      where
        x = takeWhile (==s) ss
        y = dropWhile (==s) ss