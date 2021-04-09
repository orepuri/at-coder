import Data.Char

main :: IO ()
main = do
  [a, b, c] <- words <$> getLine
  putStrLn $ if last a == head b && last b == head c then "YES" else "NO"