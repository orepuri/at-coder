import Data.List
import Data.Char

main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine :: IO [Int]
  s <- getLine
  putStrLn $ if s !! a == '-' && all isDigit (take a s) && all isDigit (drop (a + 1) s) then "Yes" else "No"