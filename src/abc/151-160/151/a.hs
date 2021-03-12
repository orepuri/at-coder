import Data.Char

main :: IO ()
main = do
  c <- getChar 
  putStrLn [chr (ord c + 1)]