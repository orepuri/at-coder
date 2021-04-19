main :: IO ()
main = do
  k <- readLn
  let odds = if odd k then k `div` 2 + 1 else k `div` 2
      evens = k - odds
  print $ odds * evens