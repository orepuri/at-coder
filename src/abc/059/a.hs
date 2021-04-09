import Data.Char

main :: IO ()
main = do
  ss <- words <$> getLine
  putStrLn $ map (toUpper . head) ss