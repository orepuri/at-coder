main :: IO ()
main = do
  s <- read <$> getLine :: IO Int
  putStrLn $ solve (s `div` 100)  (s `mod` 100)

solve :: Int -> Int -> String
solve fst snd
  | monthFirst && monthSecond = "AMBIGUOUS"
  | monthFirst = "MMYY"
  | monthSecond = "YYMM"
  | otherwise = "NA"
  where
    monthFirst = fst >= 1 && fst <= 12
    monthSecond = snd >= 1 && snd <= 12