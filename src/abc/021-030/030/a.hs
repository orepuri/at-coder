main :: IO ()
main = do
  [a, b, c, d] <- map read . words <$> getLine
  let twin = b / a
  let awin = d / c
  putStrLn $ if twin > awin then "TAKAHASHI" else if twin == awin then "DRAW" else "AOKI"
