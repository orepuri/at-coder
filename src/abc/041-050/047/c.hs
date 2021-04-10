import Data.List

main :: IO ()
main = do
  s <- getLine
  print $ solve s

solve :: String -> Int
solve (s:ss) = go s ss 0
  where
    go p [] acc = acc
    go p (s:ss) acc
      | p == s = go s ss acc
      | otherwise = go s ss (acc + 1)
