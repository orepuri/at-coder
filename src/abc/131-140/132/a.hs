import Data.List

main :: IO ()
main = do
  s <- getLine
  let [c1,c2,c3,c4] = sort s
  putStrLn $ if c1 == c2 && c3 == c4 && c1 /= c3 then "Yes" else "No"