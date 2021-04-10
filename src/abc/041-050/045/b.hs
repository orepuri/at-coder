main :: IO ()
main = do
  sa <- getLine
  sb <- getLine
  sc <- getLine
  putStrLn $ solve sa sb sc

solve :: String -> String -> String -> String
solve sa sb sc = turnA sa sb sc
  where
    turnA [] _ _ = "A"
    turnA (s:sa) sb sc = turn s sa sb sc
    turnB _ [] _ = "B"
    turnB sa (s:sb) sc = turn s sa sb sc
    turnC _ _ [] = "C"
    turnC sa sb (s:sc) = turn s sa sb sc
    turn s sa sb sc
      | s == 'a' = turnA sa sb sc
      | s == 'b' = turnB sa sb sc
      | s == 'c' = turnC sa sb sc
