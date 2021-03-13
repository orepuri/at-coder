import Data.Char

main :: IO ()
main = do
  [x1,x2] <- map digitToInt <$> getLine
  print $ x1 + x2