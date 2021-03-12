main :: IO ()
main = do
  [mh, mw] <- map read . words <$> getLine
  [h, w] <- map read . words <$> getLine
  print $ (mh - h) * (mw - w)
