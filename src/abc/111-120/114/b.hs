main :: IO ()
main = do
  s <- getLine
  print $ solve s

solve :: String -> Int
solve s = minimum $ map (\c -> abs $ 753 - read c ) cands
  where
    cands = map (\i -> take 3 $ drop i s) [0..length s-3]
