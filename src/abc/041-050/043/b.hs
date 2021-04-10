import Data.List

main :: IO ()
main = do
  s <- getLine
  putStrLn $ solve s

solve :: String -> String
solve = foldl' go ""
  where
    go acc '0' = acc ++ ['0']
    go acc '1' = acc ++ ['1']
    go [] 'B' = []
    go acc 'B' = init acc