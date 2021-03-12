main :: IO ()
main = do
  s <- getLine
  putStrLn $ solve s

solve :: String -> String
solve "Sunny" = "Cloudy"
solve "Cloudy" = "Rainy"
solve "Rainy" = "Sunny"