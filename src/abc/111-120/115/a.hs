main :: IO ()
main = do
  d <- readLn
  putStrLn $ case d of
    25 -> "Christmas"
    24 -> "Christmas Eve"
    23 -> "Christmas Eve Eve"
    22 -> "Christmas Eve Eve Eve"