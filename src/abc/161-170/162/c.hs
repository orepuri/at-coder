main :: IO ()
main = do
  k <- readLn
  print $ solve k

solve :: Int -> Int
solve k = sum [gcd c (gcd a b)|a <- [1..k], b <- [1..k], c <- [1..k]]

