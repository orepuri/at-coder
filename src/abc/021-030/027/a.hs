main :: IO ()
main = do
  [l1, l2, l3] <- map read . words <$> getLine :: IO [Int]
  print $ if l1 == l2 then l3 else if l2 == l3 then l1 else l2