main :: IO ()
main = do
  k <- readLn
  s <- getLine
  putStrLn $ if length s <= k then s else take k s ++ "..."