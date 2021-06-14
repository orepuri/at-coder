import Control.Monad

main :: IO ()
main = do
  [a, b, c] <- map read . words <$> getLine
  putStrLn $ solve a b c

solve :: Int -> Int -> Int -> String
solve a b c
  | evenC = if absA > absB then ">" else if absA == absB then "=" else "<"
  | otherwise = if a >= 0 then 
    if b >= 0 then
      if a > b then ">" else if a == b then "=" else "<"
    else
      ">"
  else
    if b >= 0 then
      "<"
    else
      if a > b then "<" else if a == b then "=" else ">"
  where
    evenC = even c
    absA = abs a
    absB = abs b

