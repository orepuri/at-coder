main :: IO ()
main = do
  s <- getLine
  putStrLn $ solve s

solve :: String -> String
solve s = map trans $ reverse s
  where
    trans '0' = '0'
    trans '1' = '1'
    trans '6' = '9'
    trans '8' = '8'
    trans '9' = '6'