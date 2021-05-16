import Data.List

main :: IO ()
main = do
  [a1,a2,a3] <- sort . map read . words <$> getLine
  putStrLn $ if a1 - a2 == a2 - a3 then "Yes" else "No"
