import Control.Monad

main :: IO ()
main = do
  n <- readLn :: IO Int
  ss <- replicateM n getLine
  print $ solve $ reverse ss

solve :: [String] -> Int
solve [] = 1
solve (s:ss) = if s == "AND" then solve ss else 2 ^ n + solve ss
  where
    n = length ss + 1


-- f (s1,s2,..,sn) = snの答え
-- sn = AND ->
--   xn = True  -> f (s1,s2,..,sn-1)
--   xn = False -> 0

-- sn = OR ->
--   xn = True -> 2^n
--   xn = False -> f (s1,s2,..sn-1)
