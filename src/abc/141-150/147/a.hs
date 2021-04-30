main :: IO ()
main = do
  as <- map read . words <$> getLine
  putStrLn $ if sum as >= 22 then "bust" else "win"