main :: IO ()
main = do
  n <- readLn :: IO Int
  s <- getLine
  print $ solve s

solve :: String -> Int
solve s = if s == s' then n else -1
  where
    n = (length s - 1) `div` 2
    s' = make n ""
    make n s
      | n == 0 = "b"
      | n `mod` 3 == 1 = "a" ++ make (n-1) s ++ "c"
      | n `mod` 3 == 2 = "c" ++ make (n-1) s ++ "a"
      | n `mod` 3 == 0 = "b" ++ make (n-1) s ++ "b"

-- 0 1
-- 1 3
-- 2 5
-- 3 7    