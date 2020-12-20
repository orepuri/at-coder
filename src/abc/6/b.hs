main :: IO ()
main = do
  n <- readLn :: IO Int
  print $ solve n

modulus = 10007 :: Int

solve :: Int -> Int
solve 1 = 0
solve 2 = 0
solve n = solve' n 0 0 1
  where
    solve' 3 _ _ c = c
    solve' n a b c = solve' (n - 1) b c ((a + b + c) `mod` modulus)