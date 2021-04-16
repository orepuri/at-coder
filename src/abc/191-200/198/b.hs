import Data.List

main :: IO ()
main = do
  n <- getLine
  let n' = dropWhileEnd (=='0') n
  putStrLn $ if n' == reverse n' then "Yes" else "No"
  
  