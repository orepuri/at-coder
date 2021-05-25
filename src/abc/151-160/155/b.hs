main :: IO ()
main = do
  n <- readLn :: IO Int
  as <- map read . words <$> getLine
  putStrLn
    $ if all (\n -> n `mod` 5 == 0 || n `mod` 3 == 0)
    $ filter even as then "APPROVED" else "DENIED"