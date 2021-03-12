import Data.Char

main :: IO ()
main = do
  n <- readLn
  print $ solve n

solve :: Int -> Int
solve n
  | mod3 == 0 = 0
  | mod3 == 1 = if any (\i -> i `mod` 3 == 1) ns then  -- 3で割って1余る数を1つ消す
                  if length ns <= 1 then -1 else 1
                else                                   -- 3で割って2余る数が必ず2つ以上あるのでそれらを消す
                  if length ns <= 2 then -1 else 2
  | mod3 == 2 = if any (\i -> i `mod` 3 == 2) ns then  -- 3で割って2余る数を1つ消す
                  if length ns <= 1 then -1 else 1
                else                                   -- 3で割って1余る数が必ず2つ以上あるのでそれらを消す
                  if length ns <= 2 then -1 else 2
  where
    ns = map digitToInt $ show n
    mod3 = sum ns `mod` 3