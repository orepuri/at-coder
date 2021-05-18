import Data.List

main :: IO ()
main = do
  n <- readLn
  putStrLn $ if solve n then "Yes" else "No"

solve :: Int -> Bool
solve n = not $ null [1|c <- [0..25],d <- [0..15],4*c+7*d==n]
