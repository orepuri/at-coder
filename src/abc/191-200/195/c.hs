main :: IO ()
main = do
  n <- readLn
  print $ solve n

solve :: Int -> Int
solve n
  | n < 1000  = 0
  | n < 10^6  = n - (10 ^ 3) + 1
  | n < 10^9  = 2 * (n - 10 ^ 6 + 1) + solve (10^6-1)
  | n < 10^12 = 3 * (n - 10 ^ 9 + 1) + solve (10^9-1)
  | n < 10^15 = 4 * (n - 10 ^ 12 + 1) + solve (10^12-1)
  | n < 10^18 = 5 * (n - 10 ^ 15 + 1) + solve (10^15-1)

--- 1 < N < 10^15
----        1 -     999  => 0
----     1,000 - 999,999 => 1
----    1,000,000 -> 999,999,999 => 2
--- 1,000,000,000 -> 999,999,999,999 => 3
--- 1,000,000,000,000 -> 999,999,999,999,999 => 4

--- 999,999,999
--- 999,999,998
--- 999,999,997
--- ..1,000,000
----    999,999
-----   999,998
----       ... 
---     100,000 

-- 1,000,000,000,000,000

--- 27,182,818,284,590