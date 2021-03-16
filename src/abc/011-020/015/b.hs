main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- map read . words <$> getLine
  let as' =  filter (/=0) as
  print $ ceiling (fromIntegral (sum as') / fromIntegral (length as'))