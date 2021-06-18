import Data.Char
import Data.List

main :: IO ()
main = do
  n <- readLn :: IO Int
  putStrLn $ solve n

solve :: Int -> String
solve = base26
  where
    base26 x
      | x <= 26 = toC (x - 1)
      | otherwise = base26 ((x - 1) `div` 26)  ++ toC ((x - 1) `mod` 26)
    toC x = [chr $ ord 'a' + x]
