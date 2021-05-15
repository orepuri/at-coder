import Data.Char
import Text.Printf

main :: IO ()
main = do
  [a, b, c, d] <- map digitToInt <$> getLine
  putStrLn $ solve a b c d

solve :: Int -> Int -> Int -> Int -> String
solve a b c d
  | a + b + c + d == 7 = printf "%d+%d+%d+%d=7" a b c d
  | a + b + c - d == 7 = printf "%d+%d+%d-%d=7" a b c d
  | a + b - c + d == 7 = printf "%d+%d-%d+%d=7" a b c d
  | a + b - c - d == 7 = printf "%d+%d-%d-%d=7" a b c d
  | a - b + c + d == 7 = printf "%d-%d+%d+%d=7" a b c d
  | a - b + c - d == 7 = printf "%d-%d+%d-%d=7" a b c d
  | a - b - c + d == 7 = printf "%d-%d-%d+%d=7" a b c d
  | a - b - c - d == 7 = printf "%d-%d-%d-%d=7" a b c d
  
