main :: IO ()
main = do
  b <- getChar
  putChar $ case b of
    'A' -> 'T'
    'C' -> 'G'
    'G' -> 'C'
    'T' -> 'A'