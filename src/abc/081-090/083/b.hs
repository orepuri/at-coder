import Data.Char

main :: IO ()
main = do
  [n, a, b] <- map read . words <$> getLine
  let go i = (i, sum $ map digitToInt $ show i)
  print $ sum $ map fst $ filter (\(_, s) -> a <= s && s <= b) $ map go [1..n]