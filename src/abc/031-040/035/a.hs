main :: IO ()
main = do
  [w, h] <- map read . words <$> getLine
  putStrLn $ if w `mod` 16 == 0 && h `mod` 9 == 0 then "16:9" else "4:3"