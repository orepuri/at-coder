main :: IO ()
main = do
  c <- getChar
  putStrLn $ if c `elem` "aiueo" then "vowel" else "consonant"