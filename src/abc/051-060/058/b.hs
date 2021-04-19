main :: IO ()
main = do
  o <- getLine
  e <- getLine
  putStrLn $ solve o e

solve :: String -> String -> String
solve [] _ = []
solve [a] [] = [a]
solve (a:o) (b:e) = [a, b] ++ solve o e