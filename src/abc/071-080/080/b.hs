import Data.Char

main :: IO ()
main = do
  n <- readLn
  putStrLn $ if solve n then "Yes" else "No"

solve :: Int -> Bool
solve n = n `mod` sum (map digitToInt $ show n) == 0