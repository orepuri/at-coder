import Data.List

main :: IO ()
main = do
  w <- getLine
  putStrLn $ if solve w then "Yes" else "No"

solve :: String -> Bool
solve s = all (even . length) $ group $ sort s
