import Data.List

main :: IO ()
main = do
  [n, k] <- map read . words <$> getLine
  print $ solve n k

solve :: Int -> Int -> Int
solve n k = foldl' (\acc _ -> if acc `mod` 200 == 0 then acc `div` 200 else read $ show acc ++ "200") n [1..k]