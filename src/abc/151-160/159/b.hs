main :: IO ()
main = do
  s <- getLine
  putStrLn $ if solve s then "Yes" else "No"

solve :: String -> Bool
solve s = all isPalindrome [s, take ((n-1) `div` 2) s, drop ((n+3)`div`2 - 1) s]
  where
    n = length s

isPalindrome s = let (l, r) = splitAt (length s `div` 2) s
  in if length l == length r then l == reverse r else l == reverse (tail r)