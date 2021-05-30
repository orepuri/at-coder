main :: IO ()
main = do
  [n, y] <- map read . words <$> getLine
  let ans = solve n y
  putStrLn $ unwords $ map show ans

solve :: Int -> Int -> [Int]
solve n t = if null cands then [-1,-1,-1] else head cands
  where
    cands = [[x,y,z]| x <- [0..n],
      x * 10000 <= t,
      y <- [0..n-x],
      x * 10000 + y * 5000 <= t,
      let z = n - x - y,
      10000*x+5000*y+1000*z==t]
  