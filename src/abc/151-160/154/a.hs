import Data.List

main :: IO ()
main = do
  [s, t] <- words <$> getLine
  [a, b] <- map read . words <$> getLine
  u <- getLine
  let ans = if s == u then [a-1, b] else [a, b-1]
  putStrLn $ unwords $ map show ans
