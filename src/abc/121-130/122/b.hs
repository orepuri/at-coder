main :: IO ()
main = do
  s <- getLine
  print $ solve s

solve :: String -> Int
solve s = maximum $ map (\i -> length $ takeWhile isACGT $ drop i s) [0..length s - 1]
  where
    isACGT c = c `elem` "ACGT"