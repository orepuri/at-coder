main :: IO ()
main = do
  n <- readLn
  let price = floor $ n * 1.08
  putStrLn $ if price < 206 then "Yay!" else if price == 206 then "so-so" else ":("
