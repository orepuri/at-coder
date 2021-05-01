import Data.Char

main :: IO ()
main = do
  n <- getLine
  let n' = digitToInt $ last n
  putStrLn $ if n' `elem` [2,4,5,7,9] then "hon" else if n' `elem` [0,1,6,8] then "pon" else "bon"