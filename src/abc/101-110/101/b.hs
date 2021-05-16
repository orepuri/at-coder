import Data.Char

main :: IO ()
main = do
  n <- readLn
  let s = sum $ map digitToInt $ show n
  putStrLn $ if n `mod` s == 0 then "Yes" else "No"