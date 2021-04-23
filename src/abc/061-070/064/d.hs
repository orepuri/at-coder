import Data.List

main :: IO ()
main = do
  n <- readLn :: IO Int
  s <- getLine
  putStrLn $ solve s

solve :: String -> String
solve s = replicate l '(' ++ s ++ replicate r ')'
  where
    (l, r) = foldl' (\(l, r) c -> let r' = if c == ')' then r - 1 else r + 1 in if r' < 0 then (l + 1, 0) else (l, r')) (0,0) s

