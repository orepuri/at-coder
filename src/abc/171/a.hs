import Data.Char

main :: IO ()
main = do
  c <- getChar
  putStrLn $ if isUpper c then "A" else "a"