import qualified Data.IntSet as S

main :: IO ()
main = do
  s <- readLn
  print $ solve s

solve :: Int -> Int
solve s = go 0 seq S.empty
  where
    seq = iterate (\n -> if even n then n `div` 2 else 3 * n + 1) s
    go i (a:as) s
      | S.member a s = i + 1
      | otherwise = go (i + 1) as $ S.insert a s