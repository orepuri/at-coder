main :: IO ()
main = do
  s <- getLine
  putStrLn $ if last s == 's' then s ++ "es" else s ++ "s"
