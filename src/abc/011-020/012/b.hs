import Text.Printf

main :: IO ()
main = do
  n <- readLn
  let (hours, mins, secs) = solve n
  printf "%02d:%02d:%02d" hours mins secs

solve :: Int -> (Int, Int, Int)
solve n = (hours, mins, secs)
  where
    hours = n `div` 3600
    mins = (n - hours * 3600) `div` 60
    secs = n - hours * 3600 - mins * 60